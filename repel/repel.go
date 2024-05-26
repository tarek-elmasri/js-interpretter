package repl

import (
	"bufio"
	"io"

	"github.com/tarek-elmasri/compiler/lexer"
	"github.com/tarek-elmasri/compiler/parser"
)

const PROPMT = ">> "

type Repl struct {
	reader io.Reader
	writer io.Writer
}

func New(r io.Reader, w io.Writer) *Repl {
	return &Repl{
		reader: r,
		writer: w,
	}
}

func (r *Repl) Start() {
	scanner := bufio.NewScanner(r.reader)

	for {
		r.writer.Write([]byte(PROPMT))
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		if line == "exit" || line == "q" {
			break
		}

		l := lexer.New(line)
		p := parser.New(l)
		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			for _, err := range p.Errors() {
				r.writer.Write([]byte(err))
				r.writer.Write([]byte("\n"))
			}

			continue
		}

		if len(program.Statements) != 0 {
			for _, stmt := range program.Statements {
				r.writer.Write([]byte(stmt.String()))
				r.writer.Write([]byte("\n"))
			}
		}
	}

}
