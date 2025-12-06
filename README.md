# flyover

❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)

[![MELPA](https://melpa.org/packages/flyover-badge.svg)](https://melpa.org/#/flyover)

<picture>
    <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/flyover_logo.png" alt="Flyover logo" height="300" width="300">
</picture>

A modern, aesthetic overlay display for *Flycheck* and *Flymake* in Emacs. Flyover is a full featured package with tons of features for you to customize. 

## Features

- 🎨 Beautiful, customizable overlays for error display and theme aware
- 🚦 Different styles for errors, warnings, and info messages
- 🔄 Real-time overlay updates while editing
- 💡 Smart positioning and formatting of error messages
- 🎯 Efficient overlay management
- 📝 Markdown-style syntax highlighting in messages

## Screenshots

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/overlay_1.png" alt="Screenshot of overlay-usage for Emacs."/>
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/overlay_2.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

## Installation

### Manual Installation

1. Download `flyover.el`
2. Add to your load path:
```elisp
(add-to-list 'load-path "/path/to/flyover")
(require 'flyover)
```

## Configuration

### Basic Setup

```elisp
;; Enable flyover-mode globally
(add-hook 'flycheck-mode-hook #'flyover-mode)

;; Configure which error levels to display
;; Possible values: error, warning, info
(setq flyover-levels '(error warning info))  ; Show all levels
;; (setq flyover-levels '(error warning))    ; Show only errors and warnings
;; (setq flyover-levels '(error))            ; Show only errors
```

### Theme Integration

```elisp
;; Use theme colors for error/warning/info faces
(setq flyover-use-theme-colors t)

;; Adjust background lightness (lower values = darker)
(setq flyover-background-lightness 45)

;; Make icon background darker than foreground
(setq flyover-percent-darker 40)

(setq flyover-text-tint 'lighter) ;; or 'darker or nil

;; "Percentage to lighten or darken the text when tinting is enabled."
(setq flyover-text-tint-percent 50)
```

### Customizing Faces

You can customize the appearance of overlays by modifying these faces:

```elisp
(custom-set-faces
 '(flyover-error
   ((t :background "#453246"
       :foreground "#ea8faa"
       :height 0.9
       :weight normal)))
 
 '(flyover-warning
   ((t :background "#331100"
       :foreground "#DCA561"
       :height 0.9
       :weight normal)))
 
 '(flyover-info
   ((t :background "#374243"
       :foreground "#a8e3a9"
       :height 0.9
       :weight normal))))
```

## Usage

Once enabled, `flyover` will automatically display error messages as overlays below the corresponding line. The overlays will:

- Show errors in red with appropriate background
- Display warnings in yellow/orange
- Show information messages in green
- Highlight code snippets and symbols in messages
- Update in real-time as you type
- Clear automatically when errors are fixed

### Checker Configuration

```elisp
;; Choose which checkers to use (flycheck, flymake, or both)
(setq flyover-checkers '(flycheck flymake))

;; Enable debug messages
(setq flyover-debug nil)
```

### Optimization settings

```elisp
;; Time in seconds to wait before checking and displaying errors after a change
(setq flyover-debounce-interval 0.2) 
```

### Positioning settings

```elisp
;; Number of lines below the error line to display the overlay
;; Default is 1 (next line), set to 0 for same line, 2 for two lines below, etc.
(setq flyover-line-position-offset 1)
```

### Message wrapping settings

```elisp
;; Enable wrapping of long error messages across multiple lines
(setq flyover-wrap-messages t)

;; Maximum length of each line when wrapping messages
(setq flyover-max-line-length 80)
```

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/flycheck_overlay_multiline.png" alt="Display text on multiple lines"/>
</p>


## Flycheck Overlay Icons

You can customize the icons used for different types of Flycheck messages in the overlay display. These settings allow you to define custom icons for information, warning, and error messages.

### Icon settings

```elisp
;;; Icons
(setq flyover-info-icon "🛈")
(setq flyover-warning-icon "⚠")
(setq flyover-error-icon "✘")

;;; Icon padding

;;; You might want to adjust this setting if you icons are not centererd or if you more or less space.fs
(setq flyover-icon-left-padding 0.9)
(setq flyover-icon-right-padding 0.9)
```

### Customizing Error Indicators

You can customize the appearance of the error indicators using various line and arrow styles through the `flyover-virtual-line-type` variable:

```elisp
(setq flyover-virtual-line-type 'curved-dotted-arrow)

;;; Overide virtual-line-type with your own
(setq flyover-virtual-line-icon "╰──") ;;; default its nil
```

| Style Name | Display | Description |
|------------|----------|-------------|
| `nil` | | No indicator |
| `line-no-arrow` | └── | Straight line |
| `curved-line-no-arrow` | ╰── | Curved line |
| `double-line-no-arrow` | ╚══ | Double line |
| `bol` | ┗━━ | Bold line |
| `dotted-line-no-arrow` | └┈┈ | Dotted line |
| `straight-arrow` | └──► | Straight line with arrow |
| `double-line-arrow` | ╚══► | Double line with arrow |
| `bold-arrow` | ┗━━► | Bold line with arrow |
| `dotted-arrow` | └┈┈► | Dotted line with arrow |
| `curved-arrow` | ╰──► | Curved line with arrow (default) |
| `curved-bold-arrow` | ╰━━► | Curved bold line with arrow |
| `curved-double-arrow` | ╰══► | Curved double line with arrow |
| `curved-dotted-arrow` | ╰┈┈► | Curved dotted line with arrow |


### Visibility settings

```elisp
;;; Hide checker name for a cleaner UI
(setq flyover-hide-checker-name t)

;;; show at end of the line instead.
(setq flyover-show-at-eol t)

;;; Hide overlay when cursor is at same line, good for show-at-eol.
(setq flyover-hide-when-cursor-is-on-same-line t)

;;; Show an arrow (or icon of your choice) before the error to highlight the error a bit more.
(setq flyover-show-virtual-line t)

;;; Show error ID/code in the overlay (default: nil)
;;; When enabled, displays the error identifier in brackets, e.g., "Missing semicolon [E001]"
;;; Useful for looking up error codes or adding suppression comments
(setq flyover-show-error-id t)
```

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/flycheck_hide_overlay_cursor.gif" alt="Gif of showing hide cursor is on same line"/>
</p>

### Show or hide checker name

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/checker_name.png" alt="Screenshot of overlay-usage for Emacs."/>
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/no_checker_name.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

### Show under or after the faulty code

<p align="center">
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/eol.png" alt="Screenshot of overlay-usage for Emacs."/>
  <img src="https://github.com/konrad1977/flycheck-overlay/blob/main/screenshots/under.png" alt="Screenshot of overlay-usage for Emacs."/>
</p>

## Testing

The package includes comprehensive tests for the message wrapping functionality:

### Running Tests

```bash
# Run core wrapping tests (no dependencies required)
emacs -batch -l test-wrapping.el

# Run full test suite (requires flycheck)
emacs -batch -l flyover.el -l flycheck-overlay-test.el -f ert-run-tests-batch-and-exit

# Interactive testing
# In Emacs: M-x load-file RET flyover-test.el RET
# Then: M-x flyover-run-tests
```

### Test Coverage

- Message wrapping with various lengths
- Multi-line display functionality  
- Line position offset behavior
- Edge cases (empty messages, single words, exact lengths)
- Integration with flycheck error objects

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Add tests for new functionality in `flyover-test.el`
4. Commit your changes (`git commit -m 'Add some amazing feature'`)
5. Push to the branch (`git push origin feature/amazing-feature`)
6. Open a Pull Request


## Acknowledgments

- Thanks to the Flycheck team for the excellent error checking framework
- Inspired by various overlay-based error display implementations

## Author

Mikael Konradsson <mikael.konradsson@outlook.com>
  
