"""The result report may only call complete canonical equality identical."""
import copy
import unittest

from compare_results import diff_nodes, result_differences


class CanonicalEqualityTest(unittest.TestCase):
    def test_fractional_date_in_a_frame(self):
        column = dict(kind="atomic", **{"class": ["Date"]}, type="double",
                      length=1, levels=None, values=["1970-01-01"],
                      numeric_payload=["0"], names=None, attrs=None)
        baseline = dict(kind="data.frame", **{"class": ["data.frame"]},
                        names=["date"], nrow=1, row_names=None,
                        columns=[column], attrs=None)
        current = copy.deepcopy(baseline)
        current["columns"][0]["numeric_payload"] = ["0.5"]
        differences = []
        diff_nodes(baseline, current, "result", differences)
        self.assertTrue(differences)
        self.assertEqual(differences[0]["aspect"], "column_numeric_payload")

    def test_new_payload_field_cannot_be_silently_ignored(self):
        baseline = dict(kind="list", **{"class": ["list"]},
                        names=None, items=[], attrs=None, extra_payload=1)
        current = dict(baseline, extra_payload=2)
        differences = []
        diff_nodes(baseline, current, "result", differences)
        self.assertTrue(differences)
        self.assertEqual(differences[0]["aspect"], "canonical_payload")

    def test_same_linkage_with_changed_raw_pseudonyms_is_different(self):
        baseline = dict(fn=".planExecute", result={"kind": "null"},
                        volatile={"raw_token_sha256": "baseline-key"})
        current = dict(baseline, volatile={"raw_token_sha256": "different-key"})
        differences = result_differences(baseline, current)
        self.assertEqual(differences[0]["aspect"], "raw_person_tokens")
        # DP masking must never mask changed pseudonym values.
        self.assertTrue(result_differences(baseline, current, mask_dp=True))

    def test_equal_complete_canonical_forms_are_identical(self):
        node = dict(kind="list", **{"class": ["list"]},
                    names=None, items=[], attrs=None)
        differences = []
        diff_nodes(node, copy.deepcopy(node), "result", differences)
        self.assertEqual(differences, [])


if __name__ == "__main__":
    unittest.main()
