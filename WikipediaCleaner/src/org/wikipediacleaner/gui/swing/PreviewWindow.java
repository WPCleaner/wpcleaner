/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.HTMLPane;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.component.MWPaneBasicFormatter;
import org.wikipediacleaner.gui.swing.worker.ExpandTemplatesWorker;
import org.wikipediacleaner.gui.swing.worker.HtmlPreview;
import org.wikipediacleaner.i18n.GT;


/**
 * A window to expand templates and show preview.
 */
public class PreviewWindow
  extends BasicWindow
  implements HtmlPreview {

  boolean showExpand;
  boolean showPreview;
  String pageTitle;
  Page page;
  JTextField textTitle;
  private JButton    buttonUpdate;
  private JButton    buttonClose;
  MWPane  textOriginal;
  private MWPane  textExpanded;
  private HTMLPane  htmlPreview;

  /**
   * Create and display a ExpandTemplatesWindow.
   * 
   * @param page Page name.
   * @param text Page text.
   * @param showExpand True if text with expanded templates should be displayed.
   * @param showPreview True if preview should be displayed.
   * @param wikipedia Wiki.
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
              expand.pageTitle = page;
              expand.page = DataManager.getPage(wikipedia, page, null, null, null);
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
            GT._T("Expand Templates and Preview - {0}", pageTitle) :
            GT._T("Preview - {0}", pageTitle))
        :
        (showExpand ?
            GT._T("Expand Templates - {0}", pageTitle) :
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
    JLabel labelTitle = Utilities.createJLabel(GT._T("&Title"));
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
    textOriginal = new MWPane(getWikipedia(), page, this);
    textOriginal.setFormatter(new MWPaneBasicFormatter());
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
      textExpanded = new MWPane(getWikipedia(), page, this);
      textExpanded.setFormatter(new MWPaneBasicFormatter());
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
      htmlPreview = HTMLPane.createHTMLPane(null);
      JScrollPane scrollPreview = new JScrollPane(htmlPreview);
      scrollPreview.setMinimumSize(new Dimension(100, 100));
      scrollPreview.setPreferredSize(new Dimension(1000, 500));
      scrollPreview.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      constraints.fill = GridBagConstraints.BOTH;
      constraints.gridwidth = 2;
      constraints.gridx = 0;
      constraints.weightx = 1;
      constraints.weighty = 1;
      panel.add(scrollPreview, constraints);
      constraints.gridy++;
    }

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonUpdate = Utilities.createJButton(GT._T("&Update"), null);
    buttonUpdate.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionUpdate"));
    buttonPanel.add(buttonUpdate);
    buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
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
   * Action called when Update button is pressed.
   */
  public void actionUpdate() {
    textOriginal.resetAttributes();
    new ExpandTemplatesWorker(
        getWikipedia(), this, textTitle.getText(),
        textOriginal,
        showExpand ? textExpanded : null,
        showPreview ? this : null).start();
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.worker.HtmlPreview#setHtml(java.lang.String)
   */
  @Override
  public void setHtml(String text) {
    htmlPreview.setText(text);
  }
}
