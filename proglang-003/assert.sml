fun assertEquals(expected, actual) =
    (expected = actual);

fun assertTrue(actual : bool) =
    assertEquals(true, actual);

fun assertFalse(actual : bool) =
    assertEquals(false, actual);


	
	