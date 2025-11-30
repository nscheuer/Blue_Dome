{
  description = "MCDP devshell setup";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devshell.url = "github:numtide/devshell";
  };

  outputs =
    {
      self,
      nixpkgs,
      devshell,
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ devshell.overlays.default ];
      };
    in
    {
      devShells.${system}.default = pkgs.devshell.mkShell {
        name = "mcdp";

        packages = with pkgs; [
          git
          bashInteractive
        ];

        # add ./bin to PATH
        env = [
          {
            name = "PATH";
            prefix = "${self}/bin";
          }
        ];
      };
    };
}
