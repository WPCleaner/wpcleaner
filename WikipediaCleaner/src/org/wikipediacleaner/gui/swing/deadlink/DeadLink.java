/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.deadlink;

import org.apache.commons.httpclient.HttpStatus;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.contents.IntervalDecorator;


/**
 * Bean representing a dead link.
 */
public class DeadLink extends IntervalDecorator {

  /** Page title */
  private final String page;

  /** External link */
  private final PageElementExternalLink link;

  /** HTTP status returned when trying the link */
  private final Integer status;

  /** Text associated with the status */
  private final String statusText;

  /**
   * Constructor.
   * 
   * @param page Page title.
   * @param link External link.
   * @param status HTTP status.
   */
  public DeadLink(String page, PageElementExternalLink link, Integer status) {
    super(link);
    this.page = page;
    this.link = link;
    this.status = status;
    this.statusText = HttpStatus.getStatusText(status);
  }

  /**
   * Constructor.
   * 
   * @param page Page title.
   * @param link External link.
   * @param status Status text.
   */
  public DeadLink(String page, PageElementExternalLink link, String status) {
    super(link);
    this.page = page;
    this.link = link;
    this.status = null;
    this.statusText = status;
  }

  /**
   * @return Page title.
   */
  public String getPage() {
    return page;
  }

  /**
   * @return External link.
   */
  public PageElementExternalLink getLink() {
    return link;
  }

  /**
   * @return Status.
   */
  public Integer getStatus() {
    return status;
  }

  /**
   * @return Status.
   */
  public String getStatusText() {
    return statusText;
  }
}
