# tests.py
# perform some unit tests
# revision $Id: tests.py 372 2008-12-24 01:03:45Z Franz $

import os
import unittest

if __name__ == "__main__":
    relative_path = os.path.dirname(__file__)
    test_loader = unittest.TestLoader()
    suite = test_loader.discover(os.path.join('.', relative_path))
    unittest.TextTestRunner(verbosity=2).run(suite)
        


