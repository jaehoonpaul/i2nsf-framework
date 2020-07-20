all:
	python $(target)

install:
	pip install --user numpy
	sudo apt-get install python-dev libmysqlclient-dev -y
	pip install --user MySQL-python
	sudo apt-get install mysql-server -y
	mysql -u root -psecu
	python initializeDB.py

clean:
	rm -rf API/*.pyc
	rm -f *.txt
	rm -f LowLevelPolicy/*.txt
