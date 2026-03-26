import sys
import re

def check_parens(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Remove strings and comments to simplify parsing
    content = re.sub(r'"(?:\\.|[^"\\])*"', '', content) # remove strings
    content = re.sub(r';.*$', '', content, flags=re.MULTILINE) # remove comments

    stack = []
    lines = content.split('\n')
    for line_idx, line in enumerate(lines):
        for char_idx, char in enumerate(line):
            if char == '(':
                stack.append((line_idx + 1, char_idx + 1))
            elif char == ')':
                if not stack:
                    print(f"Unmatched closing parenthesis at line {line_idx + 1}, col {char_idx + 1}")
                    return
                stack.pop()

    if stack:
        print(f"Unmatched opening parenthesis at line {stack[-1][0]}, col {stack[-1][1]}")
    else:
        print("Parentheses are balanced.")

check_parens('elisp/utils.el')
