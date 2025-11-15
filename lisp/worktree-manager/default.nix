{ lib, trivialBuild, fetchFromGitHub, magit, org-jira }:

trivialBuild {
  pname = "worktree-manager";
  version = "1.0.0";

  src = ./.;

  packageRequires = [ magit org-jira ];

  meta = with lib; {
    description = "Manage git worktrees with JIRA integration";
    homepage = "https://github.com/Jylhis/jotain";
    license = licenses.gpl3Plus;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
