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
import org.wikipediacleaner.gui.swing.action.CheckCategoryLinkAction;


/**
 * An action provider for CheckCategoryLinkAction.
 */
public class CheckCategoryLinkActionProvider implements ActionProvider {

  private final EnumWikipedia fromWikipedia;
  private final EnumWikipedia toWikipedia;
  private final String title;
  private final String order;

  /**
   * @param from From Wikipedia.
   * @param to To Wikipedia.
   * @param title Category name.
   * @param order Sort order.
   */
  public CheckCategoryLinkActionProvider(
      EnumWikipedia from, EnumWikipedia to, String title, String order) {
    this.fromWikipedia = from;
    this.toWikipedia = to;
    this.title = title;
    this.order = order;
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.SimpleAction#getAction(javax.swing.text.Element, javax.swing.JTextPane)
   */
  @Override
  public Action getAction(Element element, JTextPane textPane) {
    return new CheckCategoryLinkAction(fromWikipedia, toWikipedia, title, order, element, textPane);
  }

  /**
   * @param text New text.
   * @return True if this action can give this new text.
   */
  @Override
  public boolean isPossibleReplacement(String text) {
    return false;
  }
}
