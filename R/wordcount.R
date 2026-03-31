import re

def count_words_in_qmd(filepath: str,
                       include_code: bool = False,
                       include_yaml: bool = False) -> dict:
  """
    Count words in a Quarto (.qmd) document.

    Args:
        filepath:     Path to the .qmd file.
        include_code: If True, count words inside code chunks (```{...}).
        include_yaml: If True, count words in the YAML front matter (--- block).

    Returns:
        A dict with keys:
            'total'     – words counted under the chosen options
            'prose'     – words in prose only (no code, no YAML)
            'code'      – words inside code chunks
            'yaml'      – words in the YAML front matter
    """
with open(filepath, "r", encoding="utf-8") as f:
  text = f.read()

# ── 1. Strip YAML front matter (opening --- ... ---) ──────────────────────
yaml_text = ""
yaml_pattern = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)
yaml_match = yaml_pattern.match(text)
if yaml_match:
  yaml_text = yaml_match.group(1)
text = text[yaml_match.end():]          # remainder after front matter

# ── 2. Extract fenced code chunks  ```{lang} ... ``` ──────────────────────
code_text = ""
code_pattern = re.compile(r"```+\{[^}]*\}.*?```+", re.DOTALL)  # executable chunks
bare_fence_pattern = re.compile(r"```+.*?```+", re.DOTALL)      # plain fenced blocks

code_chunks = code_pattern.findall(text)
code_text = " ".join(code_chunks)
prose_text = code_pattern.sub("", text)          # remove executable chunks
prose_text = bare_fence_pattern.sub("", prose_text)  # remove plain fenced blocks

# ── 3. Strip inline code  `...` from prose ────────────────────────────────
prose_text = re.sub(r"`[^`]+`", "", prose_text)

# ── 4. Strip markdown syntax from prose ───────────────────────────────────
prose_text = re.sub(r"^\s*#{1,6}\s*", "", prose_text, flags=re.MULTILINE)  # headings
prose_text = re.sub(r"!?\[([^\]]*)\]\([^)]*\)", r"\1", prose_text)         # links/images
prose_text = re.sub(r"[*_~]{1,3}([^*_~]+)[*_~]{1,3}", r"\1", prose_text)  # emphasis
    prose_text = re.sub(r"^\s*[-*+>|]\s*", "", prose_text, flags=re.MULTILINE) # list/blockquote markers

    # ── 5. Helper: split into words ───────────────────────────────────────────
    def _word_count(s: str) -> int:
        return len(re.findall(r"\b\w+\b", s))

    counts = {
        "prose": _word_count(prose_text),
        "code":  _word_count(code_text),
        "yaml":  _word_count(yaml_text),
    }

    total = counts["prose"]
    if include_code:
        total += counts["code"]
    if include_yaml:
        total += counts["yaml"]
    counts["total"] = total

    return counts

