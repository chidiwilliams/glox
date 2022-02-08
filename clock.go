package main

import "time"

type clock struct{}

func (c clock) arity() int {
	return 0
}

func (c clock) call(_ *interpreter, _ []interface{}) interface{} {
	return float64(time.Now().UnixMilli())
}

func (c clock) String() string {
	return "<native fn>"
}
