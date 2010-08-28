/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;


/**
 * Simple test with JTextPane
 */
public class JTextPaneTest extends JPanel {

  private static final long serialVersionUID = 6426424812124490870L;

  private JTextPane textNewSection;

  public JTextPaneTest() {
    setLayout(new BorderLayout());
    add(createTextPane(), BorderLayout.CENTER);
  }

  private Component createTextPane() {
    textNewSection = new JTextPane();
    textNewSection.setBackground(Color.WHITE);
    textNewSection.setEditable(true);
    JScrollPane scrollContents = new JScrollPane(textNewSection);
    scrollContents.setMinimumSize(new Dimension(100, 100));
    scrollContents.setPreferredSize(new Dimension(1000, 500));
    scrollContents.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    return scrollContents;
  }

  static void createAndShowGui() {
    JFrame frame = new JFrame("JTextPane test");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.add(new JTextPaneTest());
    frame.pack();
    frame.setVisible(true);
  }

  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      
      public void run() {
        try {
          System.out.println(System.getProperty("java.version"));
          System.out.println(UIManager.getSystemLookAndFeelClassName());
          UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (ClassNotFoundException e) {
          // TODO
        } catch (InstantiationException e) {
          // TODO
        } catch (IllegalAccessException e) {
          // TODO
        } catch (UnsupportedLookAndFeelException e) {
          // TODO
        }
        createAndShowGui();
      }
    });
  }
}
