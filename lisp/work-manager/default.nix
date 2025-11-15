{ lib, trivialBuild, fetchFromGitHub, magit, org-jira }:

trivialBuild {
  pname = "work-manager";
  version = "1.0.0";

  src = ./.;

  packageRequires = [ magit org-jira ];

  meta = with lib; {
    description = "Work management package with git worktree and JIRA integration";
    homepage = "https://github.com/Jylhis/jotain";
    license = licenses.gpl3Plus;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
