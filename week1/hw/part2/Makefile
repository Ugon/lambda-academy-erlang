compile:
	mkdir -p target
	erlc -b beam -o ./target ../list_utils.erl ../part1/db.erl my_db.erl

shell:
	erl -pa ./target

clean:
	rm -Rf ./target