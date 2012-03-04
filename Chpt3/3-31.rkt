#lang racket

; 3.31

; The initialization is necessary for inverter gates
; w/o this initialization, inverter gates do not 'do' anything until the input
; wire is activated