package var3.dasha;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import java.awt.*;
import java.awt.event.ActionEvent;


public class MainFrame extends JFrame {
    private static final int WIDTH =700;
    private static final int HEIGHT=500;

    private  JMenuItem  pauseMenuItem;
    private  JMenuItem resumeMenuItem;
    private  JMenuItem magnetismOnMenuItem;
    private  JMenuItem magnetismOffMenuItem;

    public Field field=new Field();

    // Конструктор главного окна приложения
    public MainFrame(){
        super("Програмирование и синхронизация потоков и шарики");
        setSize(WIDTH,HEIGHT);
        Toolkit kit = Toolkit.getDefaultToolkit();
        setLocation((kit.getScreenSize().width - WIDTH)/2,
                (kit.getScreenSize().height - HEIGHT)/2);

        // Установить начальное состояние окна развѐрнутым на весь экран
        setExtendedState(MAXIMIZED_BOTH);

        // Создать меню
        JMenuBar menuBar=new JMenuBar();
        setJMenuBar(menuBar);
        JMenu ballMenu=new JMenu("Мячики");
        Action addBallAction = new AbstractAction( " Добавить мячик") {
            @Override
            public void actionPerformed(ActionEvent e) {
               field.addBall();
               if(!pauseMenuItem.isEnabled() &&
                !resumeMenuItem.isEnabled()){
                   // Ни один из пунктов меню не являются
                    // доступными - сделать доступным "Паузу"
                   pauseMenuItem.setEnabled(true);
                   magnetismOnMenuItem.setEnabled(true);
                   magnetismOffMenuItem.setEnabled(false);
               }
            }
        };
        menuBar.add(ballMenu);
        ballMenu.add(addBallAction);
        JMenu controlMenu = new JMenu("Управление");
        menuBar.add(controlMenu);
        Action pauseAction = new AbstractAction("Приостановить движение") {
            @Override
            public void actionPerformed(ActionEvent e) {
                field.pause();
                pauseMenuItem.setEnabled(false);
                resumeMenuItem.setEnabled(true);
                magnetismOnMenuItem.setEnabled(true);
                magnetismOffMenuItem.setEnabled(false);
            }
        };
        pauseMenuItem=controlMenu.add(pauseAction);
        pauseMenuItem.setEnabled(false);
        Action resumeAction =new AbstractAction("Возобновить движение") {
            @Override
            public void actionPerformed(ActionEvent e) {
                field.resume();
                pauseMenuItem.setEnabled(true);
                resumeMenuItem.setEnabled(false);
                magnetismOnMenuItem.setEnabled(true);
                magnetismOffMenuItem.setEnabled(false);
            }
        };
        resumeMenuItem=controlMenu.add(resumeAction);
        resumeMenuItem.setEnabled(false);
        Action magnetismOn =new AbstractAction("включить магнетизм") {
            @Override
            public void actionPerformed(ActionEvent e) {
                   field.magnetismTrue();
                magnetismOnMenuItem.setEnabled(false);
                magnetismOffMenuItem.setEnabled(true);
            }
        };
        magnetismOnMenuItem=controlMenu.add(magnetismOn);
        magnetismOnMenuItem.setEnabled(false);
        Action magnetismOff =new AbstractAction("выключить магнетизм") {
            @Override
            public void actionPerformed(ActionEvent e) {
                field.magnetismFalse();
                magnetismOffMenuItem.setEnabled(false);
                magnetismOnMenuItem.setEnabled(true);
            }
        };
        magnetismOffMenuItem=controlMenu.add(magnetismOff);
        magnetismOffMenuItem.setEnabled(false);
        // Добавить в центр граничной компоновки поле Field
        getContentPane().add(field, BorderLayout.CENTER);
    }

    public static void main(String[] args) {
// Создать и сделать видимым главное окно приложения
        MainFrame frame = new MainFrame();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }

}
