/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.wikipediacleaner.i18n.GT;


/**
 * An action for searching/finding text
 */
@SuppressWarnings("serial")
public class FindTextAction extends TextAction {

  private static String lastSearch = "";

  private String search;
  private JTextPane textPane;

  public FindTextAction() {
    this(null, null);
  }

  /**
   * @param search Text to search.
   * @param textPane Text pane.
   */
  public FindTextAction(String search, JTextPane textPane) {
    super("FindText");
    this.search = search;
    this.textPane = textPane;

  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    JTextComponent text = (textPane != null) ? textPane : getTextComponent(e);
    String currentSearch = search;
    if ((currentSearch == null) || (currentSearch.isEmpty())) {
      currentSearch = text.getSelectedText();
    }
    if ((currentSearch == null) || (currentSearch.isEmpty())) {
      currentSearch = lastSearch;
    }
    currentSearch = JOptionPane.showInputDialog(
        text.getParent(),
        GT._T("String to find"),
        currentSearch);
    if ((currentSearch == null) || ("".equals(currentSearch.trim()))) {
      return;
    }
    lastSearch = currentSearch;
    String textPattern = "";
    char firstChar = lastSearch.charAt(0);
    if (Character.isLetter(firstChar)) {
      textPattern =
          "[" + Character.toUpperCase(firstChar) + Character.toLowerCase(firstChar) + "]" +
          Pattern.quote(lastSearch.substring(1));
    } else {
      textPattern = Pattern.quote(lastSearch);
    }
    Pattern pattern = Pattern.compile(textPattern);
    Matcher matcher = pattern.matcher(text.getText());
    if (matcher.find(text.getCaretPosition())) {
      text.setCaretPosition(matcher.start());
      text.moveCaretPosition(matcher.end());
      text.requestFocus();
      return;
    }
    if (matcher.find(0)) {
      text.setCaretPosition(matcher.start());
      text.moveCaretPosition(matcher.end());
      text.requestFocus();
      return;
    }
  }

}
