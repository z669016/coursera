fun assertBoolEquals(expected : bool, actual : bool) =
    (expected = actual);

fun assertTrue(actual : bool) =
    assertBoolEquals(true, actual);

fun assertFalse(actual : bool) =
    assertBoolEquals(false, actual);

fun assertIntEquals(expected : int, actual : int) =
    (expected = actual);

fun assertEquals(expected, actual) =
    (expected = actual);

	
	