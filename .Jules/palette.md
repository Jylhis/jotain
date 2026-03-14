## 2024-05-24 - Auto-select help windows in Emacs
**Learning:** In Emacs, help windows (e.g., from `C-h f`) appear but do not receive focus by default, forcing users to manually switch to them to scroll or dismiss. This breaks flow.
**Action:** Adding `(help-window-select t)` to the built-in `help` package configuration fixes this, making the help window immediately active so it can be scrolled and quickly closed with `q`.
