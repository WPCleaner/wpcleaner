/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * A simple class to configure replacement of template parameters. 
 */
public class TemplateReplacement {

  private final String originalParameter;
  private final String linkParameter;
  private final String textParameter;
  
  public TemplateReplacement(String originalParameter, String linkParameter, String textParameter) {
    this.originalParameter = originalParameter;
    this.linkParameter = linkParameter;
    this.textParameter = textParameter;
  }
  
  public String getOriginalParameter() {
    return originalParameter;
  }
  
  public String getLinkParameter() {
    return linkParameter;
  }
  
  public String getTextParameter() {
    return textParameter;
  }
}
