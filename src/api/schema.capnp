@0xb253d50afc6d7304;

interface Log {
  write @0 (msg :Text);
}

interface Solver {
  solve @0 (request :Text, log :Log) -> (response :Text);
}