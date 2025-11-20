# Smoke tests for CI/CD
{ pkgs
, jotainEmacs
, ...
}:

pkgs.runCommand "jotain-smoke-tests"
{
  buildInputs = [ jotainEmacs ];
} ''
  # Test that Emacs loads
  ${jotainEmacs}/bin/emacs --batch --eval "(message \"Emacs loads successfully\")" 2>&1 | tee test-output.txt
  grep -q "Emacs loads successfully" test-output.txt

  # Test that use-package is available
  ${jotainEmacs}/bin/emacs --batch --eval "(require 'use-package) (message \"use-package available\")" 2>&1 | tee test-output.txt
  grep -q "use-package available" test-output.txt

  touch $out
  echo "Smoke tests passed!"
''
