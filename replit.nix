{pkgs}: {
  deps = [
    pkgs.R
    pkgs.rPackages.plumber
    pkgs.rPackages.jsonlite
    pkgs.rPackages.future
    pkgs.rPackages.promises
    pkgs.rPackages.ggplot2
    pkgs.rPackages.logger
    pkgs.rPackages.DBI
    pkgs.rPackages.RPostgres
    pkgs.libcurl
    pkgs.openssl
    pkgs.postgresql
  ];
}
