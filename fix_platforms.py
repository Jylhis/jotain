with open('elisp/platforms.el', 'r') as f:
    content = f.read()

import re

new_content = re.sub(
    r'\(set-face-attribute \'default nil\n\s+:font available-font\n\s+:height height\)\n\s+\(set-frame-font \(font-spec :name available-font :size \(/ height 10\.0\)\) nil t\)\n\s+\(message "Platform: Using font %s at height %d" available-font height\)',
    r'(require \'fonts)\n      (jotain-fonts--set-face-font \'default (cons available-font height))',
    content
)

with open('elisp/platforms.el', 'w') as f:
    f.write(new_content)
