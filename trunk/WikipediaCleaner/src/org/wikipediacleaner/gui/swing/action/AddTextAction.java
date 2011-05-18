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
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiConstants;


/**
 * An action listener for replacing text.
 */
@SuppressWarnings("serial")
public class AddTextAction extends TextAction {

  private final String prefix;
  private final String suffix;
  private final String url;
  private final String question;
  private final String[] possibleValues;
  private final boolean onlyList;
  private final String defaultValue;
  private final String unauthorizedCharacters;
  private final Element element;
  private final JTextPane textPane;

  private final static int BUFFER_SIZE = 1024;
  private final static int MAXIMUM_SIZE = 10000;

  public AddTextAction(
      String prefix,
      String suffix,
      String url,
      String question,
      String defaultValue,
      String unauthorizedCharacters,
      Element element,
      JTextPane textPane) {
    this(
        prefix, suffix, url, question,
        null, false, defaultValue,
        unauthorizedCharacters, element, textPane);
  }

  public AddTextAction(
      String prefix,
      String suffix,
      String url,
      String question,
      String[] possibleValues,
      boolean onlyList,
      String defaultValue,
      String unauthorizedCharacters,
      Element element,
      JTextPane textPane) {
    super("ReplaceLink");
    this.prefix = prefix;
    this.suffix = suffix;
    this.url = url;
    this.question = question;
    this.possibleValues = possibleValues;
    this.onlyList = onlyList;
    this.defaultValue = defaultValue;
    this.unauthorizedCharacters = unauthorizedCharacters;
    this.element = element;
    this.textPane = textPane;
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
    String value = null;
    if (url != null) {
      GetMethod method = null;
      InputStream is = null;
      try {
        HttpClient httpClient = new HttpClient();
        method = new GetMethod(url);
        System.out.println(url);
        int statusCode = httpClient.executeMethod(method);
        if (statusCode == HttpStatus.SC_OK) {
          is = method.getResponseBodyAsStream();
          StringBuilder html = new StringBuilder();
          byte[] tmpBytes = new byte[BUFFER_SIZE];
          int size;
          int totalSize = 0;
          Pattern pTitle = Pattern.compile("<title>(.*?)</title>", Pattern.CASE_INSENSITIVE);
          while ((value == null) &&
                 ((size = is.read(tmpBytes)) > 0) &&
                 (totalSize < MAXIMUM_SIZE)) {
            totalSize += size;
            String tmp = " " + new String(tmpBytes, 0, size);
            tmp = tmp.replaceAll("\\s", " ");
            html.append(tmp);
            Matcher m = pTitle.matcher(html);
            if (m.find() == true) {
              value = m.group(1).trim();
              value = value.replaceAll("&#232;", "è");
              value = value.replaceAll("&#233;", "é");
            }
          }
        }
      } catch (IOException ex) {
        // Nothing to do
      } catch (Exception ex) {
        // Nothing to do
      } finally {
        if (is != null) {
          try {
            is.close();
          } catch (IOException ex) {
            // Nothing to do 
          }
        }
        if (method != null) {
          method.releaseConnection();
        }
      }
    }
    if (value == null) {
      value = defaultValue;
    }
    if (possibleValues != null) {
      value = Utilities.askForValue(
          (localTextPane != null) ? localTextPane.getParent() : null,
          question,
          possibleValues, onlyList, value, unauthorizedCharacters);
    } else {
      value = Utilities.askForValue(
          (localTextPane != null) ? localTextPane.getParent() : null,
          question, value, unauthorizedCharacters);
    }
    if ((value != null) && (!value.isEmpty())) {
      StringBuilder newText = new StringBuilder();
      if (prefix != null) {
        newText.append(prefix);
      }
      newText.append(value);
      if (suffix != null) {
        newText.append(suffix);
      }
      replace(newText.toString(), localElement, localTextPane);
    }
  }

  /**
   * Replace text. 
   */
  private void replace(
      String localNewText,
      Element localElement,
      JTextPane localTextPane) {
    if ((localElement == null) ||
        (localTextPane == null) ||
        (localNewText == null)) {
      return;
    }

    // Initialize
    int startOffset = localElement.getStartOffset();
    int endOffset = localElement.getEndOffset();
    Object uuid = localElement.getAttributes().getAttribute(MediaWikiConstants.ATTRIBUTE_UUID);
    if (uuid != null) {
      boolean finished;
      do {
        finished = true;
        Element tmpElement = localTextPane.getStyledDocument().getCharacterElement(startOffset - 1); 
        if ((tmpElement != null) && (tmpElement.getAttributes() != null)) {
          if ((localElement != tmpElement) &&
              (uuid.equals(tmpElement.getAttributes().getAttribute(MediaWikiConstants.ATTRIBUTE_UUID)))) {
            startOffset = tmpElement.getStartOffset();
            finished = false;
          }
        }
      } while (!finished);
      do {
        finished = true;
        Element tmpElement = localTextPane.getStyledDocument().getCharacterElement(endOffset);
        if ((tmpElement != null) && (tmpElement.getAttributes() != null)) {
          if ((localElement != tmpElement) &&
              (uuid.equals(tmpElement.getAttributes().getAttribute(MediaWikiConstants.ATTRIBUTE_UUID)))) {
            endOffset = tmpElement.getEndOffset();
            finished = false;
          }
        }
      } while (!finished);
    }

    // Replace
    try {
      localTextPane.getDocument().remove(startOffset, endOffset - startOffset);
      localTextPane.getDocument().insertString(startOffset, localNewText, localElement.getAttributes());
      localTextPane.setCaretPosition(startOffset);
      localTextPane.moveCaretPosition(startOffset + localNewText.length());
    } catch (BadLocationException e1) {
      // Nothing to be done
    }
  }
}
