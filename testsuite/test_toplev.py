# test_toplev.py

"""revision $Id: test_toplev.py 321 2006-10-10 16:22:00Z Franz $
TESTS:
BatchStar
AgendaChanged
Assert
Run
Save
Load
SaveFacts
LoadFacts
BSave
BLoad
Build
Eval

"""
from test import *

file01 = """
(defrule duck-rule "The Duck Rule"
    ?f <- (duck)
=>
    (retract ?f)
    (assert (quack)))
"""


class Toplevel(CTestCase):
    """test Class objects"""

    def test_Top_01(self):
        """Testing: BatchStar, Save, BSave"""
        f = open(os.path.join(self.result_path, "t.clp"), 'w')
        f.write(file01)
        f.close()
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.BatchStar(os.path.join(self.result_path, "t.clp"))
            e.Assert("(duck)")
            self.assertTrue(e.AgendaChanged())
            e.Run()
            self.assertEqual(e.FactList()[-1].CleanPPForm(), "(quack)")
            e.Save(os.path.join(self.result_path, "i_%s_c.dat" % x))
            e.BSave(os.path.join(self.result_path, "i_%s_c.bdat" % x))

    def test_Top_02(self):
        """Testing: Load, SaveFacts"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.Load(os.path.join(self.result_path, "i_%s_c.dat" % x))
            e.Assert("(duck)")
            self.assertTrue(e.AgendaChanged())
            e.Run()
            self.assertEqual(e.FactList()[-1].CleanPPForm(), "(quack)")
            e.SaveFacts(os.path.join(self.result_path,"i_%s_f.dat" % x))

    def test_Top_03(self):
        """Testing: LoadFacts"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.LoadFacts(os.path.join(self.result_path, "i_%s_f.dat" % x))
            self.assertEqual(e.FactList()[-1].CleanPPForm(), "(quack)")

    def test_Top_04(self):
        """Testing: BLoad"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.BLoad(os.path.join(self.result_path, "i_%s_c.bdat" % x))
            e.Assert("(duck)")
            self.assertTrue(e.AgendaChanged())
            e.Run()
            self.assertEqual(e.FactList()[-1].CleanPPForm(), "(quack)")

    def test_Top_05(self):
        """Testing: Build"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            e.Build(file01)
            e.Assert("(duck)")
            self.assertTrue(e.AgendaChanged())
            e.Run()
            self.assertEqual(e.FactList()[-1].CleanPPForm(), "(quack)")

    def test_Top_06(self):
        """Testing: Eval"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            i = e.Eval("(+ 1 1)")
            self.assertEqual(int(i), 2)

    def test_Top_07(self):
        """Testing: Call"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            i1 = e.Call('+', "1 1")
            i2 = e.Call('+', (1, 1))
            s1 = e.Call('sym-cat', "egg spam")
            s2 = e.Call('sym-cat', ("egg", "spam"))
            self.assertEqual(int(i1), 2)
            self.assertEqual(int(i2), 2)
            self.assertEqual(str(s1), "eggspam")
            self.assertEqual(str(s2), "eggspam")

    def test_Top_08(self):
        """Testing: SaveInstances, LoadInstances"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            i1 = C.BuildInstance("i1")
            i2 = C.BuildInstance("i2")
            e.SaveInstances(os.path.join(self.result_path,"i_%s_inst.dat" % x))
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            e.LoadInstances(os.path.join(self.result_path, "i_%s_inst.dat" % x))
            self.assertEqual(e.FindInstance("i1").Name, "i1")
            self.assertEqual(e.FindInstance("i2").Name, "i2")

    def test_Top_09(self):
        """Testing: BSaveInstances, BLoadInstances"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            i1 = C.BuildInstance("i1")
            i2 = C.BuildInstance("i2")
            e.BSaveInstances(os.path.join(self.result_path, "i_%s_inst.bdat"%x))
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            e.BLoadInstances(os.path.join(self.result_path, "i_%s_inst.bdat"%x))
            self.assertEqual(e.FindInstance("i1").Name, "i1")
            self.assertEqual(e.FindInstance("i2").Name, "i2")

    def test_Top_10(self):
        """Testing: Class.BuildInstance, FindInstanceLocal"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            i1 = C.BuildInstance("i1")
            self.assertEqual(e.FindInstanceLocal("i1").Name, i1.Name)

    def test_Top_11(self):
        """Testing: RestoreInstancesFromString"""
        for x in list(self.envdict.keys()):
            e = self.envdict[x]
            e.Clear()
            e.Reset()
            C = e.BuildClass("C", "(is-a USER)")
            e.RestoreInstancesFromString("([i1] of C) ([i2] of C)")
            self.assertTrue(e.FindInstance("i1"))
            self.assertTrue(e.FindInstance("i2"))

    def test_TopCurrentEnvironment_01(self):
        """Testing: CurrentEnvironment, Environment.SetCurrent,
        Environment.Index"""
        clips.Clear()
        clips.Assert("(duck)")
        ce = clips.CurrentEnvironment()
        e = clips.Environment()
        self.assertTrue(e.Index != ce.Index)
        e.SetCurrent()
        clips.Reset()
        ce.SetCurrent()
        f = clips.FactList()[0]
        self.assertEqual(f.CleanPPForm(), "(duck)")


if __name__ == "__main__":
    unittest.main()
