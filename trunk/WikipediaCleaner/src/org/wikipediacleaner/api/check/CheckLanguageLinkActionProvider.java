/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.CheckLanguageLinkAction;


/**
 * An action provider for CheckLanguageLinkAction.
 */
public class CheckLanguageLinkActionProvider implements ActionProvider {

  private final EnumWikipedia fromWikipedia;
  private final EnumWikipedia toWikipedia;
  private final String title;
  private final String text;

  /**
   * @param from Wiki on which we need to check the language link.
   * @param to Wiki to which we need to check the language link.
   * @param title Article's title.
   * @param text Text of the link.
   */
  public CheckLanguageLinkActionProvider(
      EnumWikipedia from, EnumWikipedia to,
      String title, String text) {
    this.fromWikipedia = from;
    this.toWikipedia = to;
    this.title = title;
    this.text = text;
  }

  /**
   * @param element Text element.
   * @param textPane Text component.
   * @return Action.
   */
  @Override
  public Action getAction(Element element, JTextPane textPane) {
    return new CheckLanguageLinkAction(
        fromWikipedia, toWikipedia, title, text, element, textPane);
  }

  /**
   * @param newText New text.
   * @return True if this action can give this new text.
   */
  @Override
  public boolean isPossibleReplacement(String newText) {
    return false;
  }
}
