/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * A class for describing a matcher for template.
 */
public abstract class TemplateMatcher {

  private final EnumWikipedia wikipedia;
  private final String templateName;
  private final String explanation;
  private final boolean good;
  private final boolean helpNeeded;

  /**
   * @param wikipedia Wikipedia.
   * @param templateName Template name.
   * @param explanation Explanation.
   * @param isGood Is good ?
   * @param helpNeeded Is help needed ?
   */
  public TemplateMatcher(
      EnumWikipedia wikipedia,
      String templateName, String explanation,
      boolean isGood, boolean helpNeeded) {
    this.wikipedia = wikipedia;
    this.templateName = (templateName != null) ? CharacterUtils.ucFirst(templateName) : null;
    this.explanation = explanation;
    this.good = isGood;
    this.helpNeeded = helpNeeded;
  }

  /**
   * @param page Page.
   * @param template Template being analyzed.
   * @return Link (if any) created by the template for this matcher.
   */
  public abstract String linksTo(Page page, PageElementTemplate template);

  /**
   * @param page Page.
   * @param template Template.
   * @return List of possible kinds of replacements.
   */
  public abstract List<String> getReplacements(Page page, PageElementTemplate template);

  /**
   * @param page Page.
   * @param template Template.
   * @param index Replacement index.
   * @param text Replacement text.
   * @return Full replacement.
   */
  public abstract String getReplacement(
      Page page, PageElementTemplate template,
      int index, String text);

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @return Template name.
   */
  public String getTemplateName() {
    return templateName;
  }

  /**
   * @return Explanation.
   */
  public String getExplanation() {
    return explanation;
  }

  /**
   * @return Is good ?
   */
  public boolean isGood() {
    return good;
  }

  /**
   * @return Is help needed ?
   */
  public boolean isHelpNeeded() {
    return helpNeeded;
  }
}
