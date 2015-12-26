compile:
	mkdir -p target
	erlc -b beam -o ./target ../list_utils.erl db.erl

shell:
	erl -pa ./target

clean:
	rm -Rf ./target