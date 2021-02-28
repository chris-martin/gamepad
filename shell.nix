let
    pkgs = import <nixpkgs> {};
    haskell = pkgs.haskellPackages.ghcWithPackages hsPackageSelection;
    hsPackageSelection = haskellPackages: with haskellPackages; [
        base async containers GLUT unordered-containers
        hashable relude StateVar vector
    ];
    ghcid = pkgs.haskellPackages.ghcid;
in
    pkgs.mkShell {
        buildInputs = [ haskell ghcid ];
    }
