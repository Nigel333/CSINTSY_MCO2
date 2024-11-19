import org.jpl7.Query;

public class Front {
    public static void main(String[] args) {
        // Load the Prolog file
        System.out.println("testing");
        System.out.println(System.getProperty("java.library.path"));
        String prologFile = "consult('example1.pl').";
        Query query = new Query(prologFile);

        if (query.hasSolution()) {
            System.out.println("Prolog file loaded successfully.");
        } else {
            System.out.println("Failed to load Prolog file.");
        }

        // Query for safe cells
        Query safeQuery = new Query("safe(X, Y)");
        while (safeQuery.hasMoreSolutions()) {
            System.out.println("Safe cell: " + safeQuery.nextSolution());
        }
    }
}