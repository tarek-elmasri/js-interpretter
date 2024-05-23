package repl

import (
	"bufio"
	"fmt"
	"io"

	"github.com/tarek-elmasri/compiler/lexer"
)

const PROPMT = ">> "

type Repl struct {
	reader io.Reader
}

func New(r io.Reader) *Repl {
	return &Repl{
		reader: r,
	}
}

func (r *Repl) Start() {
	scanner := bufio.NewScanner(r.reader)

	for {
		fmt.Printf(PROPMT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		for t := l.NextToken(); t.TokenType != lexer.EOF; t = l.NextToken() {
			fmt.Printf("%+v\n", t)
		}
	}

}
