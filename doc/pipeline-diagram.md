# OCaml-docs-ci

###### tags: `ocurrent` `tarides` `summary`

## Links:

- [ocaml-docs-ci GitHub](https://github.com/ocurrent/ocaml-docs-ci)
- [Voodoo Github](https://github.com/ocaml-doc/voodoo)

## Abstract

First, the purpose of this `ocurrent` pipeline is to compile the documentation of every package in the "opamverse". To do so, it generates what we call a dependency universe. For each package (alongside with the version), the documentation is related to the packages whom our package has been compiled with. For each package, we have to compute at least a dependency universe in the form of a hash of all the (package, version) used to build it.

Once we have computed our universes, we select the one with the largest number of dependencies and we use `odoc` to generate the `html` documentation and push it to [docs-data.ocaml.org](docs-data.ocaml.org). For example, for `irmin.2.7.2`, the documentation is stored [here](https://docs-data.ocaml.org/live/p/irmin/2.7.2/doc/Irmin/index.html). This `html` is then used by [ocaml.org](ocaml.org) to display the documentation to the user in the package explorer system.

The following schema are going through the build step in a more detailled way.

## Schema

### General schema

This is the global representation of the pipeline executed by `ocaml-docs-ci`. The next steps are a split version of it, as it's too big to be understandable at one.

```mermaid
stateDiagram-v2
    opam: Opam_repo
    note left of opam
        Get the commit hash from the Github repository
        and catch commit for Voodoo.
    end note
    track: Track
    note left of track
        Compute the list of the available package in
        opam with their version.
    end note
    solver: Solver
    note left of solver
        For each tuple (package, version), it computes
        the dependencies. 
    end note
    set: Set
    note left of set
        Generate a set of all the packages with multiple
        universes for each (package, version).
    end note
    jobs: Jobs
    note left of jobs
        Generate a maximal set to build packages to reduce
        the number of jobs to use to build the docs.
    end note
    specs: Specs
    note left of specs
        Generate an OBuilder spec which is an equivalence of
        taking a subset of the set generate in the previous
        step.
    end note
    extract: Extract results
    note left of extract
        For each package, it extracts the result computed
        from the jobs.
    end note
    union: Union
    note left of union
        Merge all the packages to get a mega universe.
    end note
    bless: Bless packages
    note left of bless
        For each package, it selects one universe:
        the one with the maximal number of dependencies.
    end note
    odoc: ODoc generation
    note left of odoc
        For each Prep.t, it schedules a job to run `odoc` to
        build the documentation on a remote worker.
    end note
    html: Update Html
    note left of html
        Update, in an atomic way, the link to the
        documentation to avoid breaking.
    end note

    

    state Server {
        [*] --> opam
        opam --> track: git hash
        track --> solver: { package, digest } list
        solver --> set: { opam package, universe, commit} list
        set --> jobs: { opam package, universe, commit } set
        jobs --> specs: { package, package list} list
        specs --> extract: { base, hash, package, result } map
        extract --> union: { base, hash, package, result} map
        union --> bless: { base, hash, package, result} map
        bless --> odoc: { package, universe } map
        odoc --> html: { package, html } list
        html --> [*]: { package, blessing, hash } map
    }

    state Jobs {
        job_1: descr 1
        note right of job_1
            Install Voodo
            Install package from within the universe
            Execute Voodoo
            Rsync the result and compute the hash
        end note
        job_2: desrc 2
        note right of job_2
            Download the computed tar
            Build the doc
        end note    
    }

    job_1 --> specs: data hash or FAILED
    specs --> job_1: job specification
    job_2 --> odoc: job specification
    odoc --> job_2: odoc directories hashes
```

### Track the packages from opam

```mermaid
stateDiagram-v2
    opam: Opam_repo
    note left of opam
        Get the commit hash from the Github repository
        and catch commit for Voodoo.
    end note
    track: Track
    note left of track
        Compute the list of the available package in
        opam with their version.
    end note
    
    
    [*] --> opam
    opam --> track: git hash
    track --> [*]: { package, digest } list
```

### Compute the set of dependencies

```mermaid 
stateDiagram-v2
    solver: Solver
    note left of solver
        For each tuple (package, version), it computes
        the dependencies.
        Two solver steps:
        - with ocaml-base-compiler >= 4.02.3 & ocaml-base-compiler < 5.0.0
        - if it fails, tries with ocaml-variants = 4.12+domains
    end note
    set: Set
    note left of set
        Generate a set of all the packages with multiple
        universes for each (package, version).
    end note
    jobs: Jobs
    note left of jobs
        Generate a maximal set to build packages to reduce
        the number of jobs to use to build the docs.
    end note
    [*] --> solver: { package, digest } list
    solver --> set: { name, version, deps_univers } list
    set --> jobs: { opam package, universe, commit } set
    jobs --> [*]: { package, package list} list
```

#### Build the packages

```mermaid
stateDiagram-v2
    specs: Specs
    note left of specs
        Generate an OBuilder spec whose job is to install
        a packet and extract artifacts for a subset of the
        resulting opam switch. Each package is extracted in
        a single prep job (it can be installed multiple times 
        though (example: dune)).
    end note
    extract: Extract results
    note left of extract
        For each package, it extracts the result computed
        from the jobs.Transformation from Prep.prep (ocluster
        job) to Prep.t Package.Map.t (individual prep result
        for each extracted package).
    end note
    job_1: OWorker
    note right of job_1
        Install Voodo
        Install package from within the universe
        Execute Voodoo
        Rsync the result and compute the hash
    end note
    
    
    [*] --> specs: { package, package list} list
    specs --> extract: { base, hash, package, result } map
    extract --> [*]: { base, hash, package, result } map
    job_1 --> specs: data hash or FAILED
    specs --> job_1: job specification
```

### Select the packages and the universe

```mermaid
stateDiagram-v2
    union: Union
    note left of union
        Merge all the packages to get a mega universe.
    end note
    bless: Bless packages
    note left of bless
        For each package, it selects one universe:
        the one with the maximal number of dependencies.
    end note
    [*] --> union: { base, hash, package, result} map
    union --> bless: { base, hash, package, result} map
    bless --> [*]: { package, universe } map
```

### Build the documentation and upload on docs-data.ocaml.org

```mermaid
stateDiagram-v2
    odoc: ODoc generation
    note left of odoc
        For each Prep.t, it schedules a job to run `odoc` to
        build the documentation on a remote worker. A compilation job
        for a package will depend on the prep job and the recursive 
        compilation jobs for the package dependencies. It generates a
        compilation graph (similar to the opam dependency graph).
    end note
    html: Update Html
    note left of html
        Update, in an atomic way, the link to the
        documentation to avoid breaking.
    end note
    job_2: OWorker
    note right of job_2
        Download the computed tar and
        build the doc.
    end note

    [*] --> odoc: { package, universe } map
    odoc --> html: { package, html } list
    html --> [*]: { package, blessing, hash } map
    job_2 --> odoc: job specification
    odoc --> job_2: odoc directories hashes
```
