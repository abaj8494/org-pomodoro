# Org-Pomodoro (abaj8494 fork)

Pomodoro technique implementation for Emacs org-mode.

This fork extends the original [org-pomodoro](https://github.com/marcinkoziej/org-pomodoro) with additional features for power users.

## Fork Changes

### Version 3.2.1
- TTS backend choice: `say` (instant) or `edge-tts` (high quality)
- Default backend is now macOS native `say` for zero latency

### Version 3.2.0
- Text-to-speech announcements via edge-tts
- Configurable voice and speech rate
- Announces timer start, completion, breaks, overtime, and kill events

### Version 3.1.0
- Multiple simultaneous named pomodoros
- Prompt for pomodoro name on start
- Verbose logging to Messages buffer
- Prefix argument support for custom durations
  - `C-u`: prompt for work length
  - `C-u C-u`: prompt for work and break lengths

## Installation

### Via straight.el

```elisp
(straight-use-package
 '(org-pomodoro :type git :host github :repo "abaj8494/org-pomodoro"))
```

### Via use-package with straight

```elisp
(use-package org-pomodoro
  :straight (:host github :repo "abaj8494/org-pomodoro"))
```

## Usage

1. Move point to an org heading
2. Call `M-x org-pomodoro`
3. Enter a name for the pomodoro when prompted
4. Timer starts and clocks into the task

### Commands

| Command | Description |
|---------|-------------|
| `org-pomodoro` | Start a new pomodoro or manage existing |
| `org-pomodoro-list` | List active pomodoros, select to kill |
| `org-pomodoro-status` | Show status of all active pomodoros |

### Text-to-Speech

Enable TTS announcements:

```elisp
(setq org-pomodoro-tts-enabled t)
```

**Backend options:**

1. **macOS `say` (default)** - Instant, uses system voice:
   ```elisp
   (setq org-pomodoro-tts-backend 'say)
   (setq org-pomodoro-tts-say-voice "Daniel")  ; optional, run `say -v ?` for voices
   ```

2. **edge-tts** - High quality Microsoft voices (requires network):
   ```bash
   pip install edge-tts
   ```
   ```elisp
   (setq org-pomodoro-tts-backend 'edge-tts)
   (setq org-pomodoro-tts-voice "en-US-AndrewNeural")  ; run `edge-tts --list-voices`
   ```

## Configuration

```elisp
;; Durations (minutes)
(setq org-pomodoro-length 25)
(setq org-pomodoro-short-break-length 5)
(setq org-pomodoro-long-break-length 20)
(setq org-pomodoro-long-break-frequency 4)

;; Sounds
(setq org-pomodoro-play-sounds t)
(setq org-pomodoro-start-sound-p t)

;; TTS
(setq org-pomodoro-tts-enabled t)
(setq org-pomodoro-tts-backend 'say)        ; or 'edge-tts
(setq org-pomodoro-tts-say-voice "Daniel")  ; for 'say backend
(setq org-pomodoro-tts-voice "en-US-AndrewNeural")  ; for 'edge-tts backend
(setq org-pomodoro-tts-rate "+0%")          ; for 'edge-tts backend

;; Behavior
(setq org-pomodoro-manual-break nil)
(setq org-pomodoro-ask-upon-killing t)
```

## License

GNU General Public License v3.0
