;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-mixer
  (:use #:cl #:cffi)
  (:nicknames #:sdl-mixer)
  (:documentation "The main package of `lispbuilder-sdl-mixer'.")
  (:export

   ;; There are exports from mixer.lisp
   #:SDL-MIXER-MAJOR-VERSION
   #:SDL-MIXER-MINOR-VERSION
   #:SDL-MIXER-PATCH-LEVEL
   #:MIX-MAJOR-VERSION
   #:MIX-MINOR-VERSION
   #:MIX-PATCH-LEVEL
   #:Mix-Linked-Version
   #:MIX-CHANNELS
   #:MIX-DEFAULT-FREQUENCY
   #:MIX-DEFAULT-CHANNELS
   #:MIX-MAX-VOLUME
   #:Mix-Chunk
   #:allocated
   #:abuf
   #:alen
   #:volume
   #:Mix-Fading
   #:Mix-Music-Type
   #:Mix-Open-Audio
   #:Mix-Allocate-Channels
   #:Mix-Query-Spec
   #:Mix-Load-WAV-RW
   #:Mix-Load-MUS
   #:Mix-Load-MUS-RW
   #:Mix-Quick-Load-WAV
   #:Mix-Quick-Load-RAW
   #:Mix-Free-Chunk
   #:Mix-Free-Music
   #:Mix-Get-Music-Type
   #:Mix-Set-Post-Mix
   #:Mix-Hook-Music
   #:Mix-Hook-Music-Finished
   #:Mix-Get-Music-Hook-Data
   #:Mix-Channel-Finished
   #:MIX-CHANNEL-POST
   #:Mix-Register-Effect
   #:Mix-Unregister-Effect
   #:Mix-Unregister-All-Effects
   #:Mix-Set-Panning
   #:Mix-Set-Position
   #:Mix-Set-Distance
   #:Mix-Set-Reverse-Stereo
   #:Mix-Reserve-Channels
   #:Mix-Group-Channel
   #:Mix-Group-Channels
   #:Mix-Group-Available
   #:Mix-Group-Count
   #:Mix-Group-Oldest
   #:Mix-Group-Newer
   #:Mix-Play-Channel-Timed
   #:Mix-Play-Music
   #:Mix-Fade-In-Music
   #:Mix-Fade-In-Music-Pos
   #:Mix-Fade-In-Channel-Timed
   #:Mix-Volume
   #:Mix-Volume-Chunk
   #:Mix-Volume-Music
   #:Mix-Halt-Channel
   #:Mix-Halt-Group
   #:Mix-Halt-Music
   #:Mix-Expire-Channel
   #:Mix-Fade-Out-Channel
   #:Mix-Fade-Out-Group
   #:Mix-Fade-Out-Music
   #:Mix-Fading-Music
   #:Mix-Fading-Channel
   #:Mix-Pause
   #:Mix-Resume
   #:Mix-Paused
   #:Mix-Pause-Music
   #:Mix-Resume-Music
   #:Mix-Rewind-Music
   #:Mix-Paused-Music
   #:Mix-Set-Music-Position
   #:Mix-Playing
   #:Mix-Playing-Music
   #:Mix-Set-Music-CMD
   #:Mix-Set-Synchro-Value
   #:Mix-Get-Synchro-Value
   #:Mix-Get-Chunk
   #:Mix-Close-Audio
   

   #:SDL-MIXER-VERSION
   #:MIX-VERSION
   #:MIX-DEFAULT-FORMAT
   #:Mix-Load-WAV
   #:MIX-EFFECTS-MAX-SPEED
   #:Mix-Play-Channel
   #:Mix-Fade-In-Channel
   #:Mix-Get-Error))