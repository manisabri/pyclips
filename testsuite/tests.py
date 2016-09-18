# tests.py
# perform some unit tests
# revision $Id: tests.py 188 2004-11-10 20:04:34Z Franz $

import os
import unittest

if __name__ == "__main__":
    relative_path = os.path.dirname(__file__)
    test_loader = unittest.TestLoader()
    suite = test_loader.discover(os.path.join('.', relative_path))
    unittest.TextTestRunner(verbosity=2).run(suite)

