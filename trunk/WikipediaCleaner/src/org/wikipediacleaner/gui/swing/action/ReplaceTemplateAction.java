/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JTextPane;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TemplateParameter;
import org.wikipediacleaner.api.data.TemplateReplacement;
import org.wikipediacleaner.gui.swing.component.MenuCreator;
import org.wikipediacleaner.gui.swing.component.MediaWikiPaneFormatter;


/**
 * An action listener for replacing templates.
 */
@SuppressWarnings("serial")
public class ReplaceTemplateAction extends TextAction {

  private final String template;
  private List<TemplateParameter> parameters;
  private final TemplateReplacement replacement;
  private final String oldTitle;
  private final String newTitle;
  private final Element element;
  private final JTextPane textPane;
  private final boolean fullReplacement;

  public ReplaceTemplateAction(
      String template,
      List<TemplateParameter> parameters,
      TemplateReplacement replacement,
      String oldTitle,
      String newTitle,
      Element element,
      JTextPane textPane,
      boolean fullReplacement) {
    super("ReplaceTemplate");
    this.template = template;
    this.parameters = parameters;
    this.replacement = replacement;
    this.oldTitle = oldTitle;
    this.newTitle = newTitle;
    this.element = element;
    this.textPane = textPane;
    this.fullReplacement = fullReplacement;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    JTextPane localTextPane = textPane;
    if (localTextPane == null) {
      JTextComponent textComponent = getTextComponent(e);
      if (textComponent instanceof JTextPane) {
        localTextPane = (JTextPane) textComponent;
      }
    }
    Element localElement = element;
    if ((localTextPane != null) && (localElement == null)) {
      localElement = localTextPane.getStyledDocument().getCharacterElement(
          localTextPane.getSelectionStart());
    }
    String localOldTitle = oldTitle;
    if ((localElement != null) && (localOldTitle == null)) {
      Object attrPage = localElement.getAttributes().getAttribute(MediaWikiPaneFormatter.ATTRIBUTE_PAGE);
      if (attrPage instanceof Page) {
        localOldTitle = ((Page) attrPage).getTitle();
      }
    }
    String localNewTitle = newTitle;
    if ((localOldTitle != null) && (localNewTitle == null)) {
      localNewTitle = MenuCreator.getLastReplacement(localOldTitle);
    }
    replace(localOldTitle, localNewTitle, localElement, localTextPane);
  }

  /**
   * Replace template. 
   */
  private void replace(
      String localOldTitle,
      String localNewTitle,
      Element localElement,
      JTextPane localTextPane) {
    if ((localElement != null) &&
        (localTextPane != null) &&
        (localNewTitle != null) &&
        (localNewTitle.length() > 0)) {
      localTextPane.setCaretPosition(localElement.getStartOffset());
      localTextPane.moveCaretPosition(localElement.getEndOffset());
      StringBuilder newText = new StringBuilder();
      newText.append("{{");
      newText.append(template);
      boolean notFound = false;
      int paramNumber = 0;
      List<String> parametersDone = new ArrayList<String>(parameters.size() + 1);
      while (!notFound) {
        paramNumber++;
        notFound = true;
        for (TemplateParameter parameter : parameters) {
          if (Integer.toString(paramNumber).equals(parameter.getName())) {
            parametersDone.add(parameter.getName());
            notFound = false;
            newText.append("|");
            if (fullReplacement) {
              if (parameter.getName().equals(replacement.getOriginalParameter())) {
                newText.append(localNewTitle);
              } else {
                newText.append(parameter.getValue());
              }
            } else {
              if (parameter.getName().equals(replacement.getLinkParameter())) {
                newText.append(localNewTitle);
                newText.append("|");
                newText.append(replacement.getTextParameter());
                parametersDone.add(replacement.getTextParameter());
                newText.append("=");
                newText.append(parameter.getValue());
              } else if (parameter.getName().equals(replacement.getTextParameter())) {
                newText.append(parameter.getValue());
                newText.append("|");
                newText.append(replacement.getLinkParameter());
                parametersDone.add(replacement.getLinkParameter());
                newText.append("=");
                newText.append(localNewTitle);
              } else {
                newText.append(parameter.getValue());
              }
            }
          }
        }
      }
      for (TemplateParameter parameter : parameters) {
        if (!parametersDone.contains(parameter.getName())) {
          newText.append("|");
          newText.append(parameter.getName());
          parametersDone.add(parameter.getName());
          newText.append("=");
          newText.append(parameter.getValue());
        }
      }
      newText.append("}}");
      localTextPane.replaceSelection(newText.toString());
      MenuCreator.addLastReplacement(localOldTitle, localNewTitle);
    }
  }
}
