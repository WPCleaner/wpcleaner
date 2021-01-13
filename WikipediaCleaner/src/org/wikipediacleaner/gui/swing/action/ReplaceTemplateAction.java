/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JTextPane;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.wikipediacleaner.api.data.LinkReplacement;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TemplateParameter;
import org.wikipediacleaner.api.data.TemplateReplacement;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.gui.swing.component.MWPaneFormatter;


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
  @Override
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
      Object attrPage = localElement.getAttributes().getAttribute(MWPaneFormatter.ATTRIBUTE_PAGE);
      if (attrPage instanceof Page) {
        localOldTitle = ((Page) attrPage).getTitle();
      }
    }
    String localNewTitle = newTitle;
    if ((localOldTitle != null) && (localNewTitle == null)) {
      localNewTitle = LinkReplacement.getLastReplacement(localOldTitle);
    }
    replace(localOldTitle, localNewTitle, localElement, localTextPane);
  }

  /**
   * Replace template. 
   * 
   * @param localOldTitle Old title.
   * @param localNewTitle New title.
   * @param localElement Element.
   * @param localTextPane Text pane.
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
      TemplateBuilder builder = TemplateBuilder.from(template);
      boolean notFound = false;
      int paramNumber = 0;
      List<String> parametersDone = new ArrayList<>(parameters.size() + 1);
      while (!notFound) {
        paramNumber++;
        notFound = true;
        for (TemplateParameter parameter : parameters) {
          if (Integer.toString(paramNumber).equals(parameter.getName())) {
            parametersDone.add(parameter.getName());
            notFound = false;
            if (fullReplacement) {
              if (parameter.getName().equals(replacement.getOriginalParameter())) {
                builder.addParam(localNewTitle);
              } else {
                builder.addParam(parameter.getValue());
              }
            } else {
              if (parameter.getName().equals(replacement.getLinkParameter())) {
                builder.addParam(localNewTitle);
                builder.addParam(replacement.getTextParameter(), parameter.getValue());
                parametersDone.add(replacement.getTextParameter());
              } else if (parameter.getName().equals(replacement.getTextParameter())) {
                builder.addParam(parameter.getValue());
                builder.addParam(replacement.getLinkParameter(), localNewTitle);
                parametersDone.add(replacement.getLinkParameter());
              } else {
                builder.addParam(parameter.getValue());
              }
            }
          }
        }
      }
      for (TemplateParameter parameter : parameters) {
        if (!parametersDone.contains(parameter.getName())) {
          builder.addParam(parameter.getName(), parameter.getValue());
          parametersDone.add(parameter.getName());
        }
      }
      localTextPane.replaceSelection(builder.toString());
      LinkReplacement.addLastReplacement(localOldTitle, localNewTitle);
    }
  }
}
