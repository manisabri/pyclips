import clips
import gc
import os
import unittest


class CTestCase(unittest.TestCase):
    """base class for pyclips unit test cases"""

    def __init__(self, *args, **kwargs):
        relative_path = os.path.dirname(__file__)
        self.result_path = os.path.join(".", relative_path, "results")
        super(CTestCase, self).__init__(*args, **kwargs)

    def setUp(self):
        """set up testing environment"""
        e1 = clips.Environment()
        self.envdict = {
            b'clips': clips,
            b'env': e1,
        }
        clips.DebugConfig.WatchAll()
        e1.DebugConfig.WatchAll()

    def tearDown(self):
        clips.DebugConfig.UnwatchAll()
        self.envdict[b'env'].DebugConfig.UnwatchAll()
        s = clips.TraceStream.Read()
        fc = open(os.path.join(self.result_path, "trace.out"), 'a')
        fc.write("=" * 78 + "\n")
        fc.write("--> %s\n" % self.__class__.__name__)
        fc.write("-" * 78 + "\n")
        fc.write("%s" % s)
        fc.write("\n\n\n")
        fc.close()
        s = clips.ErrorStream.Read()
        fc = open(os.path.join(self.result_path, "error.out"), 'a')
        fc.write("=" * 78 + "\n")
        fc.write("--> %s\n" % self.__class__.__name__)
        fc.write("-" * 78 + "\n")
        fc.write("%s" % s)
        fc.write("\n\n\n")
        fc.close()
        o = gc.collect()
        fc = open(os.path.join(self.result_path, "garbage.out"), 'a')
        fc.write("%s --> %s unreached objects\n" % (
            self.__class__.__name__, o))
        fc.close()
