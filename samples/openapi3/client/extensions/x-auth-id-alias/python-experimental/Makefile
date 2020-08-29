SETUP_OUT=*.egg-info

clean:
		rm -rf $(SETUP_OUT)
		rm -rf .tox
		rm -rf .coverage
		find . -name "*.py[oc]" -delete
		find . -name "__pycache__" -delete

test: clean
		python3 -m pip install tox
		tox
