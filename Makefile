
CHAPTERS := 000_header.md \
			01_Introduction.md \
			02_anatomy_of_a_function.md \
			03_environment_inside_function_call.md \
			04_expressions_and_environments.md \
			05_manipulation_of_expressions.md \
			06_quotes_and_substitution.md \
			


SOURCE_CHAPTERS := $(foreach chapter,$(CHAPTERS),chapters/$(chapter))


book.pdf: $(SOURCE_CHAPTERS) Makefile
	(cd pdf_book && make CHAPTERS="$(CHAPTERS)")
	cp pdf_book/book.pdf book.pdf

print_book.pdf: $(SOURCE_CHAPTERS) Makefile pdf_book/Makefile
	(cd pdf_book && make print_book.pdf CHAPTERS="$(CHAPTERS)")
	cp pdf_book/print_book.pdf print_book.pdf

book.epub:  $(SOURCE_CHAPTERS) Makefile
	(cd ebook && make CHAPTERS="$(CHAPTERS)")
	cp ebook/book.epub book.epub

book.mobi: book.epub
	./kindlegen book.epub -o book.mobi

all: book.pdf book.epub book.mobi

clean:
	rm book.pdf book.epub book.mobi

