import re
from gui.pc import mf_tab_tk


def test_profiles_feature_removed_marker():
    """SC-TST-000: Profiles feature was removed in this UI variant.
    Verify the source contains a comment indicating removal so the test documents
    that the profile manager is not present in this variant.
    """
    src = mf_tab_tk.__doc__ or ''
    # also check file contents for explicit comment
    import inspect
    src_text = inspect.getsource(mf_tab_tk)
    assert 'Profiles feature removed' in src_text or 'Profiles UI removed' in src_text
