/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.analysis;


/**
   * Bean for holding internal links count.
 */
public class InternalLinkCount {

  /**
   * Link name.
   */
  private final String link;

  /**
   * Count of internal links.
   */
  private int internalLinkCount;

  /**
   * Count of internal links marked as good.
   */
  private int goodInternalLinkCount;

  /**
   * Count of internal links where help is requested.
   */
  private int helpNeededInternalLinkCount;

  /**
   * Count of good links through templates.
   */
  private int goodTemplateCount;

  /**
   * Count of help needed links through templates.
   */
  private int helpNeededTemplateCount;

  /**
   * Count of incorrect links through templates.
   */
  private int incorrectTemplateCount;

  /**
   * @param link Link name.
   */
  InternalLinkCount(String link) {
    this.link = link;
  }

  /**
   * @return Link name.
   */
  public String getLink() {
    return link;
  }

  /**
   * @return Total count of links.
   */
  public int getTotalLinkCount() {
    return internalLinkCount +
        goodInternalLinkCount +
        helpNeededInternalLinkCount +
        goodTemplateCount +
        helpNeededTemplateCount +
        incorrectTemplateCount;
  }

  /**
   * Increase the count of internal links.
   */
  public void addInternalLink() {
    internalLinkCount++;
  }

  /**
   * @return Count of internal links.
   */
  public int getInternalLinkCount() {
    return internalLinkCount;
  }

  /**
   * Increase the count of good internal links.
   */
  public void addGoodInternalLink() {
    goodInternalLinkCount++;
  }

  /**
   * @return Count of good internal links.
   */
  public int getGoodInternalLinkCount() {
    return goodInternalLinkCount;
  }

  /**
   * Increase the count of good links through templates.
   */
  public void addGoodTemplateLink() {
    goodTemplateCount++;
  }

  /**
   * @return Count of good links through templates.
   */
  public int getGoodTemplateCount() {
    return goodTemplateCount;
  }

  /**
   * Increase the count of internal links with help needed.
   */
  public void addHelpNeededInternalLink() {
    helpNeededInternalLinkCount++;
  }

  /**
   * Increase the count of help needed links through templates.
   */
  public void addHelpNeededTemplateLink() {
    helpNeededTemplateCount++;
  }

  /**
   * @return Count of links with help needed.
   */
  public int getHelpNeededCount() {
    return helpNeededTemplateCount + helpNeededInternalLinkCount;
  }

  /**
   * Increase the count of incorrect links through templates.
   */
  public void addIncorrectTemplateLink() {
    incorrectTemplateCount++;
  }

  /**
   * @return Count of incorrect links through templates.
   */
  public int getIncorrectTemplateCount() {
    return incorrectTemplateCount;
  }
}
