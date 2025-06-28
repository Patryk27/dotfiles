{ ... }: {
  containers = {
    grafana = import ./containers/grafana.nix;
  };
}
