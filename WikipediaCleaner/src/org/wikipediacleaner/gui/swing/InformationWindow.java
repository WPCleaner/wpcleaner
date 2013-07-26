/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.io.IOException;
import java.io.StringReader;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.lobobrowser.html.HtmlRendererContext;
import org.lobobrowser.html.UserAgentContext;
import org.lobobrowser.html.gui.HtmlPanel;
import org.lobobrowser.html.parser.DocumentBuilderImpl;
import org.lobobrowser.html.test.SimpleUserAgentContext;
import org.wikipediacleaner.api.constants.EnumWikipedia;
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
  JLabel lblTitle;
  HtmlPanel textInformation;
  UserAgentContext ucontextInformation;
  HtmlRendererContext rcontextInformation;
  private JButton buttonClose;

  /**
   * Create and display an InformationWindow.
   * 
   * @param title Title.
   * @param information Information.
   * @param wikipedia Wikipedia.
   */
  public static void createInformationWindow(
      final String title,
      final String information,
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
      DocumentBuilderImpl dbi = new DocumentBuilderImpl(
          ucontextInformation, rcontextInformation);
      InputSource is = new InputSource(new StringReader(information));
      is.setSystemId("http://localhost");
      textInformation.setDocument(dbi.parse(is), rcontextInformation);
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
    textInformation = new HtmlPanel();
    ucontextInformation = new SimpleUserAgentContext();
    rcontextInformation = new MWHtmlRendererContext(textInformation, ucontextInformation);
    textInformation.setPreferredSize(new Dimension(500, 500));
    textInformation.setMinimumSize(new Dimension(100, 100));
    lblTitle.setLabelFor(textInformation);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weighty = 1;
    constraints.weightx = 1;
    panel.add(textInformation, constraints);
    constraints.gridy++;

    // Buttons
    JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
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
