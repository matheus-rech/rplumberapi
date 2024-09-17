{pkgs}: {
  deps = [
    pkgs.R
    (pkgs.rWrapper.override {
      packages = with pkgs.rPackages; [
        plumber
        jsonlite
        future
        promises
        ggplot2
        logger
        DBI
        RPostgres
      ];
    })
    pkgs.postgresql
  ];
}
