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

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.StringReader;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.lobobrowser.html.HtmlRendererContext;
import org.lobobrowser.html.UserAgentContext;
import org.lobobrowser.html.gui.HtmlPanel;
import org.lobobrowser.html.parser.DocumentBuilderImpl;
import org.lobobrowser.html.test.SimpleUserAgentContext;
import org.w3c.dom.Document;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiHtmlRendererContext;
import org.wikipediacleaner.gui.swing.worker.ExpandTemplatesWorker;
import org.wikipediacleaner.gui.swing.worker.HtmlPreview;
import org.wikipediacleaner.i18n.GT;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


/**
 * A window to expand templates and show preview.
 */
public class PreviewWindow
  extends BasicWindow
  implements ActionListener, HtmlPreview {

  private final static String ACTION_CLOSE  = "CLOSE";
  private final static String ACTION_UPDATE = "UPDATE";

  boolean showExpand;
  boolean showPreview;
  String page;
  JTextField textTitle;
  private JButton    buttonUpdate;
  private JButton    buttonClose;
  JTextPane  textOriginal;
  private JTextPane  textExpanded;
  private HtmlPanel  htmlPreview;
  UserAgentContext ucontext;
  HtmlRendererContext rcontext;

  /**
   * Create and display a ExpandTemplatesWindow.
   * 
   * @param page Page name.
   * @param text Page text.
   */
  public static void createExpandTemplatesWindow(
      final String page,
      final String text,
      final boolean showExpand,
      final boolean showPreview,
      final EnumWikipedia wikipedia) {
    createWindow(
        "ExpandTemplatesWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        PreviewWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof PreviewWindow) {
              PreviewWindow expand = (PreviewWindow) window;
              expand.page = page;
              expand.showExpand = showExpand;
              expand.showPreview = showPreview;
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof PreviewWindow) {
              PreviewWindow expand = (PreviewWindow) window;
              expand.textTitle.setText(page);
              expand.textOriginal.setText(text);
              expand.textOriginal.setCaretPosition(0);
              expand.actionUpdate();
            }
          }
          
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return showPreview ?
        (showExpand ?
            GT._("Expand Templates and Preview - {0}", page) :
            GT._("Preview - {0}", page))
        :
        (showExpand ?
            GT._("Expand Templates - {0}", page) :
            "?");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(2, 2, 2, 2);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Discussion title
    textTitle = new JTextField(60);
    JLabel labelTitle = Utilities.createJLabel(GT._("&Title"));
    labelTitle.setLabelFor(textTitle);
    labelTitle.setHorizontalAlignment(SwingConstants.TRAILING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(labelTitle, constraints);
    constraints.gridx++;
    constraints.weightx = 1;
    panel.add(textTitle, constraints);
    constraints.gridy++;

    // Original contents
    textOriginal = new JTextPane();
    textOriginal.setBackground(Color.WHITE);
    textOriginal.setEditable(true);
    JScrollPane scrollOriginal = new JScrollPane(textOriginal);
    scrollOriginal.setMinimumSize(new Dimension(100, 100));
    scrollOriginal.setPreferredSize(new Dimension(1000, 500));
    scrollOriginal.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 1;
    panel.add(scrollOriginal, constraints);
    constraints.gridy++;

    // Expanded contents
    if (showExpand) {
      textExpanded = new JTextPane();
      textExpanded.setBackground(Color.WHITE);
      textExpanded.setEditable(false);
      JScrollPane scrollExpanded = new JScrollPane(textExpanded);
      scrollExpanded.setMinimumSize(new Dimension(100, 100));
      scrollExpanded.setPreferredSize(new Dimension(1000, 500));
      scrollExpanded.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      constraints.fill = GridBagConstraints.BOTH;
      constraints.gridwidth = 2;
      constraints.gridx = 0;
      constraints.weighty = 1;
      constraints.weightx = 1;
      panel.add(scrollExpanded, constraints);
      constraints.gridy++;
    }

    // Preview
    if (showPreview) {
      htmlPreview = new HtmlPanel();
      ucontext = new SimpleUserAgentContext();
      rcontext = new MediaWikiHtmlRendererContext(htmlPreview, ucontext);
      constraints.fill = GridBagConstraints.BOTH;
      constraints.gridwidth = 2;
      constraints.gridx = 0;
      constraints.weightx = 1;
      constraints.weighty = 1;
      panel.add(htmlPreview, constraints);
      constraints.gridy++;
    }

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonUpdate = Utilities.createJButton(GT._("&Update"));
    buttonUpdate.setActionCommand(ACTION_UPDATE);
    buttonUpdate.addActionListener(this);
    buttonPanel.add(buttonUpdate);
    buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.setActionCommand(ACTION_CLOSE);
    buttonClose.addActionListener(this);
    buttonPanel.add(buttonClose);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_UPDATE.equals(e.getActionCommand())) {
      actionUpdate();
    } else if (ACTION_CLOSE.equals(e.getActionCommand())) {
      actionClose();
    }
  }

  /**
   * Action called when Update button is pressed.
   */
  void actionUpdate() {
    new ExpandTemplatesWorker(
        getWikipedia(), this, textTitle.getText(),
        textOriginal,
        showExpand ? textExpanded : null,
        showPreview ? this : null).start();
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.worker.HtmlPreview#setHtml(java.lang.String)
   */
  public void setHtml(String text) {
    try {
      DocumentBuilderImpl dbi = new DocumentBuilderImpl(ucontext, rcontext);
      InputSource is = new InputSource(new StringReader(text));
      is.setSystemId(getWikipedia().getWikiURL() + "?title=" + textTitle.getText());
      Document document = dbi.parse(is);
      htmlPreview.setDocument(document, rcontext);
    } catch (SAXException e) {
      htmlPreview.clearDocument();
    } catch (IOException e) {
      htmlPreview.clearDocument();
    }
  }

  /**
   * Action called when Close button is pressed.
   */
  private void actionClose() {
    dispose();
  }
}
