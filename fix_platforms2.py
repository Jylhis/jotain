with open('elisp/platforms.el', 'r') as f:
    content = f.read()

content = content.replace("(require \\'fonts)", "(require 'fonts)").replace("(jotain-fonts--set-face-font \\'default", "(jotain-fonts--set-face-font 'default")

with open('elisp/platforms.el', 'w') as f:
    f.write(content)
