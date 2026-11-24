# Examples

## `sanctum-config.el`

A complete, copy-ready Emacs configuration for `herdr-sanctum` demonstrating:

- **Built-in tonal surface separation** via `herdr-sanctum-panel-surface-blend` (alongside `herdr-sanctum-panel-line-spacing` and `herdr-sanctum-panel-fringe-width`) giving `*sanctum-spaces*` and `*sanctum-drawer*` a subtle tinted panel surface and matching fringes framing the center `*sanctum*` terminal canvas
- **Cohesive `modus-themes` overrides** for `modus-vivendi-tinted` (dark) and `modus-operandi-tinted` (light), flat `tab-bar` / `mode-line`, and 1px `window-divider`
- **Soft UI integrations** (`spacious-padding`, `nerd-icons`, `vertico`, `orderless`, `ghostel`) — all optional and guarded with `require ... nil t`
- **Minimal mode-line** with a pill-styled IME indicator (`EN` / active input method) and global attention badge (`◆N ✓N`)
- **Keybindings and pre-configured companion modules** (ready to enable with `(herdr-sanctum-ime-mode 1)` / `(herdr-sanctum-tab-routing-mode 1)`)

Quick tryout from the repository root:

```elisp
(add-to-list 'load-path "/path/to/herdr-sanctum")
(load "/path/to/herdr-sanctum/examples/sanctum-config.el")
(sanctum-example-setup-fonts)
(load-theme 'modus-vivendi-tinted t) ; or 'modus-operandi-tinted
(herdr-sanctum-apply-theme t)
(sanctum-example-apply-chrome-polish)
(sanctum-example-setup-mode-line)
```
