/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.util.List;

import javax.swing.Action;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ReplaceTextAction;


/**
 * An action provider for ReplaceTextAction.
 */
class ReplaceTextActionProvider implements ActionProvider {

  private final Page page;

  private final String newText;

  private final boolean automatic;

  private final boolean automaticBot;

  /**
   * @param newText New text.
   */
  ReplaceTextActionProvider(String newText) {
    this(newText, false, false);
  }

  /**
   * @param newText New text.
   * @param automatic True if the replacement can be done automatically.
   */
  ReplaceTextActionProvider(
      String newText, boolean automatic) {
    this(newText, automatic, automatic);
  }

  /**
   * @param newText New text.
   * @param automatic True if the replacement can be done automatically.
   * @param automaticBot True if the replacement can be done automatically in bot mode.
   */
  ReplaceTextActionProvider(
      String newText, boolean automatic, boolean automaticBot) {
    this(null, newText, automatic, automaticBot);
  }

  /**
   * @param newText New text.
   * @param automatic True if the replacement can be done automatically.
   * @param automaticBot True if the replacement can be done automatically in bot mode.
   */
  ReplaceTextActionProvider(
      Page page, String newText,
      boolean automatic, boolean automaticBot) {
    this.page = page;
    this.newText = newText;
    this.automatic = automatic;
    this.automaticBot = automaticBot;
  }

  /**
   * @return New text.
   */
  public String getNewText() {
    return newText;
  }

  /**
   * @return New text.
   */
  public String getFinalizedNewText() {

    if (newText == null) {
      return null;
    }

    // Text finalization
    String localNewText = newText;
    if (page != null) {
      try {
        PageAnalysis analysis = page.getAnalysis(localNewText, false);
        List<PageElementFunction> functions = analysis.getFunctions();
        boolean parseNeeded = false;
        if (functions != null) {
          for (PageElementFunction function : functions) {
            if (function.getMagicWord() == null) {
              parseNeeded = true;
            } else if (!function.getMagicWord().isFunctionNotPSTMagicWord()) {
              parseNeeded = true;
            }
          }
        }
        if (parseNeeded) {
          API api = APIFactory.getAPI();
          localNewText = api.parseText(page.getWikipedia(), page.getTitle(), localNewText, false);
        }
      } catch (APIException e) {
        // Nothing to do
      }
    }

    return localNewText;
  }

  /**
   * @return True if the replacement can be done automatically.
   */
  public boolean isAutomatic() {
    return automatic;
  }

  /**
   * @return True if the replacement can be done automatically in bot mode.
   */
  public boolean isAutomaticBot() {
    return automaticBot;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.ActionProvider#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  @Override
  public Action getAction(Element element, JTextPane textPane) {
    return new ReplaceTextAction(page, newText, element, textPane);
  }

  /**
   * @param text New text.
   * @return True if this action can give this new text.
   */
  @Override
  public boolean isPossibleReplacement(String text) {
    return (text != null) && (text.equals(newText));
  }
}
