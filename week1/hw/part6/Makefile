compile:
	mkdir -p target
	erlc -b beam -o ./target db.erl ../part2/my_db.erl

shell:
	erl -pa ./target

clean:
	rm -Rf ./target