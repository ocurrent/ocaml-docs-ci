@0xbf46af72b205a04b;

struct JobStatus {
  id          @0 :Text;
  description @1 :Text;
  canCancel   @2 :Bool;
  canRebuild  @3 :Bool;
}

interface Job {
  status  @0 () -> JobStatus;
  cancel  @1 () -> ();
  rebuild @2 () -> (job :Job);

  log     @3 (start :Int64) -> (log :Data, next :Int64);
  # Return a chunk of log data starting at byte offset "start" and the
  # value to use for "start" in the next call to continue reading.
  # Returns 0 bytes of data to indicate end-of-file.
  # If the log is incomplete and there is no data currently available,
  # it waits until something changes before returning.
  # If "start" is negative then it is relative to the end of the log.
}

interface Engine {
  activeJobs @0 () -> (ids :List(Text));
  job        @1 (id :Text) -> (job :Job);
}

enum BuildStatus {
  notStarted @0;
  passed     @1;
  failed     @2;
  pending    @3;
}

struct JobInfo {
  variant @0 :Text; # TODO This should be a structured type of information.
  state :union {
    notStarted @1 :Void;

    passed     @2 :Void;

    failed     @3 :Text;
    # The text is the error message.

    active     @4 :Void;
    # The job is still running.

    aborted    @5 :Void;
    # This means we couldn't find any record of the job. It probably means
    # that the server crashed while building, and when it came back up we
    # no longer wanted to test that commit anyway.
  }

  queuedAt  :union {
    ts         @6 :Float64;
    # timestamp as seconds since epoch
    none       @7 :Void;
  }

  startedAt :union {
    ts         @8 :Float64;
    # timestamp as seconds since epoch
    none       @9 :Void;
  }

  finishedAt :union {
    ts         @10 :Float64;
    # timestamp as seconds since epoch
    none       @11 :Void;
  }
}

enum StepType {
  prep @0;
  depCompilePrep @1;
  depCompileCompile @2;
  compile @3;
  buildHtml @4;
}

struct Step {
  type @0 :StepType;
  # TODO This needs to link to a Job somehow? Use the job_id
  jobId @1 :Text;
}

struct StepInfo {
  type @0 :Text; # see if we can use StepType here.
  # The job_id links a step to the job that reifies it.
  jobId :union {
    id @1 :Text;
    none @2 :Void;
  }
  status @3 :BuildStatus;
  stepPackage :union { # the (optional) package that the step refers to
    name @4 :Text;
    none @5 :Void;
  }
}

struct PackageInfo {
  name @0 :Text;
}

struct PackageBuildStatus {
  version @0: Text;
  status @1 :BuildStatus;
}

interface Package {
  steps @0 (package_version :Text) -> (steps :List(StepInfo));

  versions @1 () -> (versions :List(PackageBuildStatus));
}

interface Pipeline {
  package @0 (package_name :Text) -> (package : Package);

  packages @1 () -> (packages :List(PackageInfo));
}
