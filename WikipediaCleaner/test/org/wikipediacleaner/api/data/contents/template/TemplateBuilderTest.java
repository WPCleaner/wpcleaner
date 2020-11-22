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
 * Test class for template builder.
 */
public class TemplateBuilderTest {

  @Test
  public void simpleTemplate() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE").toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE}}", template);
  }

  @Test
  public void simpleTemplateWithWhitespace() {

    // Create template
    String template = TemplateBuilder.from(" TEMPLATE ").toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{ TEMPLATE }}", template);
  }

  @Test
  public void templateWithParameters() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE")
        .addParam("VALUE1")
        .addParam(" VALUE2 ")
        .addParam("NAME3", "VALUE3")
        .addParam(" NAME4 ", " VALUE4 ").toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE|VALUE1| VALUE2 |NAME3=VALUE3| NAME4 = VALUE4 }}", template);
  }

  @Test
  public void templateWithSimpleBlockFormat() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE")
        .addParam("VALUE1")
        .addParam(" VALUE2 ")
        .addParam("NAME3", "VALUE3")
        .addParam(" LONGNAME4 ", " VALUE4 ")
        .forceBlockFormat().toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE\n| VALUE1\n| VALUE2\n| NAME3     = VALUE3\n| LONGNAME4 = VALUE4\n}}", template);
  }

  @Test
  public void templateWithBlockFormat() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE")
        .addParam("VALUE1")
        .addParam(" VALUE2 ")
        .addParam("NAME3", "VALUE3")
        .addParam(" LONGNAME4 ", " VALUE4 ")
        .forceBlockFormat(2, 3, 4, false, 5).toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE\n  |     VALUE1\n  |     VALUE2\n  |   NAME3    =     VALUE3\n  |   LONGNAME4    =     VALUE4\n}}", template);
  }

  @Test
  public void templateWithBlockFormatAligned() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE")
        .addParam("VALUE1")
        .addParam(" VALUE2 ")
        .addParam("NAME3", "VALUE3")
        .addParam(" LONGNAME4 ", " VALUE4 ")
        .forceBlockFormat(2, 3, 4, true, 5).toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE\n  |     VALUE1\n  |     VALUE2\n  |   NAME3        =     VALUE3\n  |   LONGNAME4    =     VALUE4\n}}", template);
  }

  @Test
  public void templateWithSimpleInlineFormat() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE")
        .addParam("VALUE1")
        .addParam(" VALUE2 ")
        .addParam("NAME3", "VALUE3")
        .addParam(" LONGNAME4 ", " VALUE4 ")
        .forceInlineFormat().toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE |VALUE1 |VALUE2 |NAME3=VALUE3 |LONGNAME4=VALUE4 }}", template);
  }

  @Test
  public void templateWithInlineFormat() {

    // Create template
    String template = TemplateBuilder.from("TEMPLATE")
        .addParam("VALUE1")
        .addParam(" VALUE2 ")
        .addParam("NAME3", "VALUE3")
        .addParam(" LONGNAME4 ", " VALUE4 ")
        .forceInlineFormat(2, 3, 4, 5).toString();

    // Check title
    assertNotNull(
        "template is null",
        template);
    assertEquals(
        "Template is incorrect",
        "{{TEMPLATE  |     VALUE1  |     VALUE2  |   NAME3    =     VALUE3  |   LONGNAME4    =     VALUE4  }}", template);
  }
}
