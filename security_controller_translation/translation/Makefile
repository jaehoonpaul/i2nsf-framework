all:
	python $(target)

install:
	pip install --user numpy
	apt-get install -y python-dev libmysqlclient-dev
	pip install --user MySQL-python
	apt-get install -y mysql-server
	
clean:
	rm -rf API/*.pyc
	rm -f *.txt

