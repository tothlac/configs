syntax match ELMod /\[[[:alnum:]_]\{-}:[[:digit:]]\{-}]/
syntax match ELCritical /\[critical]\c/
syntax match ELError /\[error]\c/
syntax match ELWarning /\[warning]\c/
syntax match ELKnownWarning /\[warning].*Dropping index file\c/
syntax match ELX /XXXXXXXX/
syntax match ELDebug /.*\[debug\].*\c/
hi link ELCritical Critical
hi link ELError Error
hi link ELWarning Error
hi link ELX Type
hi link ELMod Statement
hi ELDebug guifg=gray ctermfg=6
