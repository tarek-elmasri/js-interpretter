package main

import (
	"os"

	repl "github.com/tarek-elmasri/compiler/repel"
)

func main() {

	r := repl.New(os.Stdin)
	r.Start()
}
