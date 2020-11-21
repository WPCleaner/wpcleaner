/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.contents.template;

import static org.junit.Assert.*;

import org.junit.Test;


/**
 * Test class for template parameter name resolver.
 */
public class TemplateParamNameResolverTest {

  @Test
  public void completeTest() {

    // Create parameter name resolver
    TemplateParamNameResolver resolver = new TemplateParamNameResolver();

    // Check first parameter: named parameter
    String paramName = resolver.nextParameter("param1");
    assertNotNull(
        "name is null",
        paramName);
    assertEquals(
        "name is incorrect",
        "param1", paramName);

    // Check second parameter: unnamed parameter
    paramName = resolver.nextParameter(null);
    assertNotNull(
        "name is null",
        paramName);
    assertEquals(
        "name is incorrect",
        "1", paramName);

    // Check third parameter: named parameter
    paramName = resolver.nextParameter("param2");
    assertNotNull(
        "name is null",
        paramName);
    assertEquals(
        "name is incorrect",
        "param2", paramName);

    // Check fourth parameter: named parameter with name matching parameter number
    paramName = resolver.nextParameter("2");
    assertNotNull(
        "name is null",
        paramName);
    assertEquals(
        "name is incorrect",
        "2", paramName);

    // Check fifth parameter: unnamed parameter
    paramName = resolver.nextParameter(null);
    assertNotNull(
        "name is null",
        paramName);
    assertEquals(
        "name is incorrect",
        "3", paramName);

    // Check sixth parameter: unnamed parameter
    paramName = resolver.nextParameter("");
    assertNotNull(
        "name is null",
        paramName);
    assertEquals(
        "name is incorrect",
        "4", paramName);
  }
}
