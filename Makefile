format:
	@fprettify --recursive app/
	@fprettify --recursive src/
	@fprettify --recursive test/

build:
	@fpm build

run:
	@fpm run

test:
	@fpm test
