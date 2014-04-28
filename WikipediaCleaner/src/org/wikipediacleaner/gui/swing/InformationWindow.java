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
import java.io.IOException;
import java.io.StringReader;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.lobobrowser.html.HtmlRendererContext;
import org.lobobrowser.html.UserAgentContext;
import org.lobobrowser.html.gui.HtmlPanel;
import org.lobobrowser.html.parser.DocumentBuilderImpl;
import org.lobobrowser.html.test.SimpleUserAgentContext;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWHtmlRendererContext;
import org.wikipediacleaner.i18n.GT;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


/**
 * A window to show information.
 */
public class InformationWindow
  extends BasicWindow {

  String title;
  String information;
  boolean html;
  JLabel lblTitle;
  JTextPane textPane;
  HtmlPanel textInformation;
  UserAgentContext ucontextInformation;
  HtmlRendererContext rcontextInformation;
  private JButton buttonClose;

  /**
   * Create and display an InformationWindow.
   * 
   * @param title Title.
   * @param information Information.
   * @param html True if information is in HTML format.
   * @param wikipedia Wikipedia.
   */
  public static void createInformationWindow(
      final String title,
      final String information,
      final boolean html,
      final EnumWikipedia wikipedia) {
    createWindow(
        "InformationWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        InformationWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof InformationWindow) {
              InformationWindow info = (InformationWindow) window;
              info.title = title;
              info.information = information;
              info.html = html;
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof InformationWindow) {
              InformationWindow info = (InformationWindow) window;
              info.updateInformation();
            }
          }
          
        });
  }

  /**
   * Update information.
   */
  void updateInformation() {
    try {
      lblTitle.setText(title);
      if (html) {
        DocumentBuilderImpl dbi = new DocumentBuilderImpl(
            ucontextInformation, rcontextInformation);
        InputSource is = new InputSource(new StringReader(information));
        is.setSystemId("http://localhost");
        textInformation.setDocument(dbi.parse(is), rcontextInformation);
      } else {
        textPane.setText(information);
      }
    } catch (SAXException e) {
      // Nothing
    } catch (IOException e) {
      // Nothing
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Information");
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

    // Title
    lblTitle = Utilities.createJLabel(title);
    lblTitle.setHorizontalAlignment(SwingConstants.LEADING);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(lblTitle, constraints);
    constraints.gridy++;

    // Information
    Component component = null;
    if (html) {
      textInformation = new HtmlPanel();
      ucontextInformation = new SimpleUserAgentContext();
      rcontextInformation = new MWHtmlRendererContext(textInformation, ucontextInformation);
      component = textInformation;
    } else {
      textPane = new JTextPane();
      JScrollPane scrollPane = new JScrollPane(textPane);
      scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
      scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
      component = scrollPane;
    }
    component.setPreferredSize(new Dimension(500, 500));
    component.setMinimumSize(new Dimension(100, 100));
    lblTitle.setLabelFor(component);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 1;
    panel.add(component, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
    buttonPanel.add(buttonClose);
    constraints.fill = GridBagConstraints.NONE;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonPanel, constraints);
    constraints.gridy++;

    return panel;
  }
}
