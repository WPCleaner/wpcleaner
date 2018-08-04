/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.util.Collections;
import java.util.List;

import javax.swing.text.TextAction;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.StringChecker;


/**
 * An action listener for adding a preferred disambiguation.
 */
@SuppressWarnings("serial")
public class ChangePreferredDisambiguationAction extends TextAction {

  private final EnumWikipedia wikipedia;
  private final String page;
  private final String preferred;
  private final boolean add;
  private final Component parent;
  private final String message;
  private final String defaultValue;
  private final StringChecker checker;

  /**
   * Constructor.
   * 
   * @param wikipedia Wiki.
   * @param page Page.
   * @param preferred Preferred disambiguation action.
   * @param add True for adding preferred disambiguation.
   */
  public ChangePreferredDisambiguationAction(
      EnumWikipedia wikipedia,
      String page,
      String preferred,
      boolean add) {
    super("AddPreferredDisambiguation");
    this.wikipedia = wikipedia;
    this.page = page;
    this.preferred = preferred;
    this.add = add;
    this.parent = null;
    this.message = null;
    this.defaultValue = null;
    this.checker = null;
  }

  /**
   * Constructor.
   * 
   * @param wikipedia Wiki.
   * @param page Page.
   * @param parent Parent component.
   * @param message Message.
   * @param defaultValue Default value.
   * @param checker Checker.
   */
  public ChangePreferredDisambiguationAction(
      EnumWikipedia wikipedia,
      String page,
      Component parent,
      String message,
      String defaultValue,
      StringChecker checker) {
    super("AddPreferredDisambiguation");
    this.wikipedia = wikipedia;
    this.page = page;
    this.preferred = null;
    this.add = true;
    this.parent = parent;
    this.message = message;
    this.defaultValue = defaultValue;
    this.checker = checker;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    Configuration config = Configuration.getConfiguration();
    List<String> preferredDabs = config.getStringSubList(
        wikipedia, Configuration.SUB_ARRAY_PREFERRED_DAB, page);
    if (add) {
      String value = preferred;
      if (value == null) {
        value = Utilities.askForValue(parent, message, defaultValue, checker);
      }
      if ((value != null) && !preferredDabs.contains(value)) {
        preferredDabs.add(value);
        Collections.sort(preferredDabs);
        config.setStringSubList(
            wikipedia, Configuration.SUB_ARRAY_PREFERRED_DAB, page, preferredDabs);
      }
    } else {
      if ((preferred != null) && preferredDabs.contains(preferred)) {
        preferredDabs.remove(preferred);
        Collections.sort(preferredDabs);
        config.setStringSubList(
            wikipedia, Configuration.SUB_ARRAY_PREFERRED_DAB, page, preferredDabs);
      }
    }
  }
}
