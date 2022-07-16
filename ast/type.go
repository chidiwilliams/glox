package ast

type Type interface {
}

type ArrayType struct {
	Types []Type
}

type SingleType struct {
	Name        string
	GenericArgs []Type
}

type UnionType struct {
	Left  Type
	Right Type
}
