module Project = struct
  type driver_kind = None | Solo5 | Xen | Unix | Windows | OSX

  type category =
    | Core
    | Driver of driver_kind
    | Parsing
    | Logging
    | Storage
    | Network
    | Web
    | VCS
    | Security
    | Testing

  type category_description = { order : int; name : string; descr : string }

  let category_description = function
    | Core ->
        {
          order = 1;
          name = "core";
          descr =
            "The core libraries that form Mirage. These are primarily module type definitions for \
             functionality such as networking and storage, as well as the frontend configuration \
             and CLI tooling.";
        }
    | Driver None ->
        {
          order = 2;
          name = "driver";
          descr =
            "Library implementations for boot-related functionality not specific to a particular \
             target.";
        }
    | Driver Solo5 ->
        {
          order = 3;
          name = "driver/solo5";
          descr =
            "Drivers for booting on the [Solo5](https://github.com/Solo5/solo5) target, which uses \
             a slimmed down KVM hypervisor to run.";
        }
    | Driver Xen ->
        {
          order = 4;
          name = "driver/xen";
          descr = "Drivers for booting directly on the [Xen](http://xen.org) hypervisor.";
        }
    | Driver Unix ->
        {
          order = 5;
          name = "driver/unix";
          descr =
            "Drivers for running as a normal Unix process on Linux, Free/Net/OpenBSD or macos.";
        }
    | Driver Windows ->
        {
          order = 6;
          name = "driver/windows";
          descr = "Drivers for bindings to Windows-specific APIs and services.";
        }
    | Driver OSX ->
        {
          order = 7;
          name = "driver/osx";
          descr = "Drivers for bindings to macos specific APIs and services.";
        }
    | Parsing ->
        {
          order = 8;
          name = "parsing";
          descr = "Libraries to help parsing and pickling into various formats.";
        }
    | Logging ->
        {
          order = 9;
          name = "logging";
          descr =
            "Logging and profiling libraries for recording and analysing unikernel activities.";
        }
    | Storage ->
        {
          order = 10;
          name = "storage";
          descr =
            "Libraries to encode into persistent on-disk formats, often with interoperability with \
             other systems.";
        }
    | Network ->
        {
          order = 11;
          name = "network";
          descr = "Libraries that implement remote network protocols, often specified in IETF RFCs.";
        }
    | Web ->
        {
          order = 12;
          name = "web";
          descr = "Libraries that implement web-related technologies, including the HTTP protocol.";
        }
    | VCS ->
        {
          order = 13;
          name = "vcs";
          descr =
            "Version-controlled storage technologies, including the Irmin datastructure layer.";
        }
    | Security ->
        { order = 14; name = "security"; descr = "Cryptography and encryption-related libraries." }
    | Testing ->
        {
          order = 15;
          name = "testing";
          descr = "Libraries to assist with building unit tests and coverage.";
        }

  type t = { org : string; repo : string; cat : category; opam : string list; descr : string }

  let v ?(org = "mirage") repo cat opam descr = { org; repo; cat; opam; descr }

  let packages =
    [
      (*v "mirage" Core ["mirage"; "mirage-runtime"; "functoria"; "functoria-runtime"] "This is the main repository that contains the CLI tool.";
        v "ocaml-cstruct" Parsing ["cstruct"] "Map OCaml arrays onto C-like structs, suitable for parsing wire protocols.";
        v "ocaml-uri" Web ["uri"] "RFC3986 URI parsing library";
        v "irmin" VCS ["irmin"] "a library for persistent stores with built-in snapshot, branching and reverting mechanisms.";
        v ~org:"mirleft" "ocaml-tls" Security ["tls"] "a pure OCaml implementation of Transport Layer Security.";
        v "git" VCS ["git"] "Git format and protocol in pure OCaml"*)
      v "digestif" Security [ "digestif" ] "Hashing functions in pure OCaml or C.";
    ]
end

(* 
- org: mirage
  repo: ocaml-hvsock
  type: driver/win
  opam: [hvsock]
  descr: These bindings allow Host to VM communication on Hyper-V systems on both Linux and Windows.
- org: mirage
  repo: mirage-skeleton
  type: example
  descr: Examples of different types of Mirage applications.
- org: mirage
  repo: ocaml-tuntap
  type: driver/unix
  opam: [tuntap]
  descr: Bindings to the [tuntap](https://en.wikipedia.org/wiki/TUN/TAP) virtual network kernel devices, for userspace networking on Linux and macos.
- org: mirage
  repo: ocaml-cstruct
  type: parsing
  opam: [cstruct]
  descr: Map OCaml arrays onto C-like structs, suitable for parsing wire protocols.
- org: mirage
  repo: shared-block-ring
  type: storage
  opam: [shared-block-ring]
  descr: A simple persistent on-disk fixed length queue.
- org: mirage
  repo: prometheus
  type: logging
  opam: [prometheus,prometheus-app]
  descr: Report metrics to a Prometheus server.
- org: mirage
  repo: ocaml-uri
  type: web
  opam: [uri]
  descr: RFC3986 URI parsing library
- org: mirage
  repo: irmin
  type: vcs
  opam: [irmin,irmin-unix,mirage-irmin]
  descr: a library for persistent stores with built-in snapshot, branching and reverting mechanisms.
- org: mirage
  repo: ocaml-qcow
  type: storage
  opam: [qcow]
  descr: pure OCaml code for parsing, printing, modifying `.qcow` format data
- org: mirage
  repo: ocaml-dns
  type: network
  opam: [dns,mirage-dns]
  descr: a pure OCaml implementation of the DNS protocol, intended to be a reasonably high-performance implementation.
- org: mirage
  repo: ocaml-conduit
  type: network
  opam: [conduit,mirage-conduit]
  descr: a library to establish and listen for TCP and SSL/TLS connections.
- org: mirage
  repo: ocaml-tar
  type: storage
  opam: [tar]
  descr: read and write tar files, with an emphasis on streaming.
- org: mirage
  repo: ocaml-pcap
  type: network
  opam: [pcap-format]
  descr: an interface to encode and decode pcap files, dealing with both endianess, including endianess detection.
- org: mirage
  repo: mirage-tcpip
  type: network
  opam: [tcpip]
  descr: a pure OCaml implementation of the TCP/IP protocol suite.
- org: mirage
  repo: charrua-core
  type: network
  opam: [charrua-core]
  descr: a DHCPv4 server and wire frame encoder and decoder.
- org: mirage
  repo: alcotest
  type: testing
  opam: [alcotest]
  descr: a lightweight and colourful test framework that exposes simple interface to perform unit tests.
- org: mirage
  repo: ocaml-ipaddr
  type: network
  opam: [ipaddr]
  descr: a library for manipulation of IP (and MAC) address representations.
- org: mirage
  repo: mirage-block-ramdisk 
  type: storage
  opam: [mirage-block-ramdisk]
  descr: a simple in-memory block device.
- org: mirage
  repo: mirage-block
  type: core
  opam: [mirage-block,mirage-block-lwt]
  descr: generic operations over Mirage block devices.
- org: mirage
  repo: mirage-block-unix
  type: driver/unix
  opam: [mirage-block-unix]
  descr: Unix implementation of the Mirage block interface.
- org: mirage
  repo: mirage-block-xen
  type: driver/xen
  opam: [mirage-block-xen]
  descr: Client and server implementations of the Xen paravirtualised block driver protocol
- org: mirage
  repo: mirage-net-xen
  type: driver/xen
  opam: [mirage-net-xen]
  descr: Client and server implementations of the Xen paravirtualised network driver protocol
- org: mirage
  repo: ocaml-vchan
  type: driver/xen
  opam: [vchan]
  descr: implementation of the "vchan" shared-memory communication protocol.
- org: mirage
  repo: mirage-platform
  type: driver
  opam: [mirage-unix,mirage-xen-ocaml,mirage-xen]
  descr: Platform libraries for Mirage for Unix and Xen that handle timers, device setup and the main loop, as well as the runtime for the Xen unikernel.
- org: mirage
  repo: mirage-protocols
  type: core
  opam: [mirage-protocols,mirage-protocols-lwt]
  descr: a set of module types that libraries intended to be used as Mirage network implementations should implement.
- org: mirage
  repo: ocaml-cohttp
  type: web
  opam: [cohttp]
  descr: a library for creating HTTP daemons, with a portable HTTP parser and implementations using various asynchronous programming libraries.
- org: mirage
  repo: ocaml-crunch
  type: storage
  opam: [crunch]
  descr: take a directory of files and compile them into a standalone OCaml module that serves the contents directly from memory.
- org: mirage
  repo: ocaml-fat
  type: storage
  opam: [fat-filesystem]
  descr: implementation of the FAT filesystem to allow the easy preparation of bootable disk images containing kernels, and to provide a simple filesystem layer for Mirage applications.
- org: mirage
  repo: mirage-http
  type: web
  opam: [mirage-http]
  descr: a Cohttp-based webserver implementation of the Mirage HTTP interfaces.
- org: mirage
  repo: mirage-fs-unix
  type: driver/unix
  opam: [mirage-fs-unix]
  descr: a pass-through Mirage filesystem to an underlying Unix directory.
- org: mirage
  repo: mirage-bootvar-xen
  type: driver/xen
  opam: [mirage-bootvar-xen]
  descr: library for reading Mirage unikernel boot parameters from Xen.
- org: mirage
  repo: mirage-net-unix
  type: driver/unix
  opam: [mirage-net-unix]
  descr: Unix implementation of the Mirage NETWORK interface that exposes Ethernet frames via tuntap.
- org: mirage
  repo: mirage-net-macosx
  type: driver/osx
  opam: [mirage-net-macosx]
  descr: MacOSX implementation of the Mirage NETWORK interface that exposes raw Ethernet frames using the Vmnet framework available in MacOS X Yosemite onwards.
- org: mirage
  repo: mirage-entropy
  type: core
  opam: [mirage-entropy]
  descr: Reliable entropy sources for Mirage unikernels.
- org: mirage
  repo: mirage-time
  type: core
  opam: [mirage-time,mirage-time-lwt]
  descr: Module types for time-related operations in Mirage.
- org: mirage
  repo: mirage-random
  type: core
  opam: [mirage-random]
  descr: Randomness signatures for Mirage, and an implementation using the OCaml stdlib.
- org: mirage
  repo: mirage-net
  type: core
  opam: [mirage-net,mirage-net-lwt]
  descr: Network (Ethernet) signatures for Mirage.
- org: mirage
  repo: mirage-logs
  type: core
  opam: [mirage-logs]
  descr: a reporter for the [Logs](http://erratique.ch/software/logs) library that writes log messages to stderr, using a Mirage `CLOCK` to add timestamps.
- org: mirage
  repo: mirage-kv
  type: core
  opam: [mirage-kv,mirage-kv-lwt]
  descr: provides key/value store signatures that Mirage storage libraries can implement.
- org: mirage
  repo: mirage-fs
  type: core
  opam: [mirage-fs,mirage-fs-lwt]
  descr: provides filesystem module signatures that Mirage storage libraries can implement.
- org: mirage
  repo: mirage-flow
  type: core
  opam: [mirage-flow,mirage-flow-lwt]
  descr: Network flow implementations and combinators to manipulate and compose them.
- org: mirage
  repo: mirage-console
  type: core
  opam: [mirage-console-lwt,mirage-console,mirage-console-unix,mirage-console-xen-backend,mirage-console-xen-cli,mirage-console-xen-proto,mirage-console-xen]
  descr: Pure OCaml module types and implementations of Mirage consoles, for Unix and Xen.
- org: mirage
  repo: mirage-clock
  type: core
  opam: [mirage-clock,mirage-clock-freestanding,mirage-clock-lwt,mirage-clock-unix]
  descr: portable support for an operating system timesources.
- org: mirage
  repo: mirage-channel
  type: core
  opam: [mirage-channel,mirage-channel-lwt]
  descr: Channels are buffered reader/writers built on top of an unbuffered mirage-flow implementation.
- org: mirage
  repo: mirage-stack
  type: core
  opam: [mirage-stack,mirage-stack-lwt]
  descr: provides a set of module types which libraries intended to be used as Mirage network stacks should implement.
- org: mirage
  repo: mirage-device
  type: core
  opam: [mirage-device]
  descr: the signature for basic abstract devices for Mirage and a pretty-printing function for device errors
- org: mirage
  repo: ocaml-vhd
  type: storage
  opam: [vhd-format]
  descr: a pure OCaml library to read and write vhd format data, plus a simple command-line tool which allows vhd files to be interrogated, manipulated, format-converted and streamed to and from files and remote servers.
- org: mirage
  repo: ocaml-freestanding
  type: driver
  opam: [ocaml-freestanding]
  descr: a freestanding OCaml runtime suitable for linking with a unikernel base layer such as Solo5.
- org: mirage
  repo: mirage-solo5
  type: driver/solo5
  opam: [mirage-solo5]
  descr: the Mirage `OS` library for Solo5 targets, which handles the main loop and timers.
- org: mirage
  repo: mirage-block-solo5
  type: driver/solo5
  opam: [mirage-block-solo5]
  descr: Solo5 implementation of the Mirage block interface.
- org: mirage
  repo: mirage-net-solo5
  type: driver/solo5
  opam: [mirage-net-solo5]
  descr: Solo5 implementation of the Mirage network interface.
- org: mirage
  repo: mirage-bootvar-solo5
  type: driver/solo5
  opam: [mirage-bootvar-solo5]
  descr: library for passing boot-time variables to Solo5 targets.
- org: mirage
  repo: mirage-console-solo5
  type: driver/solo5
  opam: [mirage-console-solo5]
  descr: implementation of the Mirage Console interface for Solo5 targets.
- org: mirage
  repo: ocaml-github
  type: vcs
  opam: [github]
  descr: an OCaml interface to the GitHub APIv3 (JSON) that is compatible with Mirage and also compiles to pure JavaScript.
- org: mirage
  repo: ocaml-git
  type: vcs
  opam: [git,git-mirage,git-unix]
  descr: Git format and protocol in pure OCaml, with support for on-disk and in-memory Git stores.
- org: mirage
  repo: ocaml-9p
  type: storage
  opam: [9p-format]
  descr: an implementation of the 9P protocol from outer space.
- org: mirage
  repo: shared-memory-ring
  type: driver/xen
  opam: [shared-memory-ring]
  descr: a set of libraries for creating shared memory producer/consumer rings that follow the Xen hypervisor ABI for virtual devices.
- org: mirage
  repo: io-page
  type: driver
  opam: [io-page]
  descr: support for efficient handling of I/O memory pages on Unix and Xen.
- org: mirage
  repo: ocaml-evtchn
  type: driver/xen
  opam: [evtchn]
  descr: Xen event channel interface for Mirage. Event channels are the Xen equivalent of interrupts, used to signal when data is available for processing.
- org: mirage
  repo: ocaml-gnt
  type: driver/xen
  opam: [gnt]
  descr: Xen grant table bindings for OCaml that are used to create Xen device driver "backends" (servers) and "frontends" (clients).
- org: mirage
  repo: cowabloga
  type: web
  opam: [cowabloga]
  descr: a deprecated library to setup a simple blog and wiki using the Zurb Foundation CSS/HTML templates.
- org: mirage
  repo: ocaml-cow
  type: web
  opam: [cow]
  descr: OCaml combinators for HTML, XML, JSON and Markdown format handling.
- org: mirage
  repo: mirage-xen-minios
  type: driver/xen
  opam: [mirage-xen-minios]
  descr: installs the C libraries for the Xen MiniOS and OpenLibM.
- org: mirage
  repo: ocaml-named-pipe
  type: driver/win
  opam: [named-pipe]
  descr: OCaml bindings for named pipes, which are used on Windows for local (and remote) IPC.
- org: mirage
  repo: parse-argv
  type: driver
  opam: [parse-argv]
  descr: Common code for parsing argv strings that is used by the various bootvar libraries to pass configuration information to a unikernel.
- org: mirage
  repo: mirage-profile
  type: logging
  opam: [mirage-profile]
  descr: library to trace execution of OCaml/Lwt programs at the level of Lwt threads, and associated viewers to process the trace results.
- org: mirage
  repo: ocaml-xenstore
  type: driver/xen
  opam: [xenstore]
  descr: implementation of the Xenstore communication protocol, including client and server libraries.
- org: mirage
  repo: ocaml-base64
  type: parsing
  opam: [base64]
  descr: Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation.
- org: mirage
  repo: ocaml-asl
  type: logging
  opam: [asl]
  descr: library to log via the Apple System Log on macosx.
- org: mirage
  repo: mirage-tc
  type: core
  opam: [mirage-tc]
  descr: a set of functors and combinators to convert to and from and JSON values and Cstruct buffers.
- org: mirage
  repo: ocaml-mstruct
  type: parsing
  opam: [mstruct]
  descr: a mutability layer for Cstruct buffers.
- org: mirage
  repo: ocaml-hex
  type: parsing
  opam: [hex]
  descr: library providing hexadecimal converters for OCaml values.
- org: mirage
  repo: ezjsonm
  type: web
  opam: [ezjsonm]
  descr: a simple but slower parsing library for JSON values, based on jsonm.
- org: mirage
  repo: ocaml-rpc
  type: network
  opam: [rpc]
  descr: library and syntax extension to generate functions to convert values of a given type to and from theirs RPC representations.
- org: mirage
  repo: ocaml-vmnet
  type: driver/osx
  opam: [vmnet]
  descr: MacOS X bridged networking via the vmnet.framework.
- org: mirage
  repo: ocaml-launchd
  type: driver/osx
  opam: [launchd]
  descr: make services that are automatically started by the macosx launchd service.
- org: mirage
  repo: ocaml-mbr
  type: storage
  opam: [mbr]
  descr: library for manipulating Master Boot Records, to create bootable disk images and for Mirage kernels to read the partition tables on attached disks.
- org: mirage
  repo: ocaml-magic-mime
  type: web
  opam: [magic-mime]
  descr: a database of MIME types that maps filename extensions into MIME types suitable for use in many Internet protocols such as HTTP or e-mail.
- org: samoht
  repo: depyt
  type: core
  opam: [depyt]
  descr: type combinators to define runtime representation for OCaml types and generic operations to manipulate values with a runtime type representation.
- org: samoht
  repo: irmin-watcher
  type: vcs
  opam: [irmin-watcher]
  descr: Portable filesystem watch backends using FSevents or Inotify
- org: mirage
  repo: mirage-qubes
  type: driver
  opam: [mirage-qubes]
  descr: implementations of various [QubesOS](https://www.qubes-os.org) protocols.
- org: mirleft
  repo: ocaml-tls
  type: security
  opam: [tls]
  descr: a pure OCaml implementation of Transport Layer Security.
- org: mirleft
  repo: ocaml-nocrypto
  type: security
  opam: [nocrypto]
  descr: a small cryptographic library that puts emphasis on the applicative style and ease of use. It includes basic ciphers (AES, 3DES, RC4), hashes (MD5, SHA1, SHA2), public-key primitives (RSA, DSA, DH) and a strong RNG (Fortuna).
- org: mirleft
  repo: ocaml-x509
  type: security
  opam: [x509]
  descr: X.509 is a public key infrastructure used mostly on the Internet, and this library implements most parts of RFC5280 and RFC6125.
- org: mirleft
  repo: ocaml-asn1-combinators
  type: security
  opam: [asn1-combinators]
  descr: a library for expressing ASN.1 in OCaml by embedding the abstract syntax directly in the language. 

*)
