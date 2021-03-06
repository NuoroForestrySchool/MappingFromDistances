PRAGMA foreign_keys = off;

BEGIN TRANSACTION;-- Table: Dendro

CREATE TABLE Dendro (
    ID,
    [Specie ],
    c_130,
    h_tot,
    d_130
);-- Table: Interdistances

CREATE TABLE Interdistances (
    m_id    INTEGER,
    [from]  INTEGER,
    [to]    INTEGER,
    dist    REAL,
    [left]  REAL,
    [right] REAL
);-- Table: TreeCoo

CREATE TABLE TreeCoo (
    p       REAL,
    local_x REAL,
    local_y REAL,
    n       REAL,
    sx      REAL,
    sy      REAL
);-- View: AllTris

CREATE VIEW AllTris AS
    SELECT *
      FROM BasicTris
    UNION
    SELECT m_id,
           tri,
           p2,
           pl,
           p1
      FROM BasicTris
    UNION
    SELECT m_id,
           tri,
           pl,
           p1,
           p2
      FROM BasicTris;-- View: BasicTris

CREATE VIEW BasicTris AS
    SELECT m_id,
           'l' AS tri,
           [from] AS p1,
           [to] AS p2,
           [left] AS pl
      FROM Interdistances
     WHERE [left] IS NOT NULL
    UNION
    SELECT m_id,
           'r' AS tri,
           [to] AS p1,
           [from] AS p2,
           [right] AS pl
      FROM Interdistances
     WHERE [right] IS NOT NULL;-- View: CIBR

CREATE VIEW CIBR AS
    SELECT m.m_id AS m_id,
           m.[from] AS [from],
           m.[to] AS [to],
           m.dist AS dist,
           m.d_130_from AS d_130_from,
           m.d_130_to AS d_130_to,
           0.005 * (ifnull(m.d_130_from, 0) + ifnull(m.d_130_to, 0) ) AS [offset],
           m.dist + 0.005 * (ifnull(m.d_130_from, 0) + ifnull(m.d_130_to, 0) ) AS corrected_dist
      FROM (
               SELECT a.m_id,
                      a.[from],
                      a.[to],
                      a.dist,
                      a.d_130 AS d_130_from,
                      b.d_130 AS d_130_to
                 FROM (
                          SELECT a0.m_id,
                                 a0.[from],
                                 a0.[to],
                                 a0.dist,
                                 b0.d_130
                            FROM Interdistances a0
                                 LEFT JOIN
                                 (
                                     SELECT ID,
                                            d_130
                                       FROM Dendro
                                      WHERE ID NOT IN (
                                                SELECT ID
                                                  FROM Dendro
                                                 GROUP BY ID
                                                HAVING count( * ) > 1
                                            )
                                 )
                                 b0 ON a0.[from] = b0.ID
                      )
                      a
                      LEFT JOIN
                      (
                          SELECT ID,
                                 d_130
                            FROM Dendro
                           WHERE ID NOT IN (
                                     SELECT ID
                                       FROM Dendro
                                      GROUP BY ID
                                     HAVING count( * ) > 1
                                 )
                      )
                      b ON a.[to] = b.ID
           )
           m;-- View: ContradictoryTris

CREATE VIEW ContradictoryTris AS
    SELECT DISTINCT a.m_id AS m_id1,
                    a.tri AS tr1,
                    b.m_id AS m_id2,
                    b.tri AS tri2
      FROM AllTris a
           JOIN
           AllTris b ON a.pl = b.pl AND 
                        a.p1 = b.p2 AND 
                        a.p2 = b.p1
     WHERE a.m_id < b.m_id;-- View: ContradictoryTrisX

CREATE VIEW ContradictoryTrisX AS
    SELECT a.*,
           '||' [-A-],
           x.*,
           '||' [-B-],
           b.*
      FROM ContradictoryTris x
           JOIN
           Interdistances a ON m_id1 = a.m_id
           JOIN
           Interdistances b ON m_id2 = b.m_id;-- View: DegeneratedTris

CREATE VIEW DegeneratedTris AS
    SELECT *
      FROM Interdistances
     WHERE [from] = [to] OR 
           [from] = [left] OR 
           [from] = [right] OR 
           [to] = [left] OR 
           [to] = [right] OR 
           [left] = [right];-- View: DoubleMeasures

CREATE VIEW DoubleMeasures AS
    SELECT a.*
      FROM Interdistances a
           JOIN
           (
               SELECT [from],
                      [to]
                 FROM Q_Interdistances
                GROUP BY [from],
                         [to]
               HAVING count( * ) > 1
           )
           b ON a.[from] = b.[from] AND 
                a.[to] = b.[to];-- View: LonelyP

CREATE VIEW LonelyP AS
    SELECT *
      FROM TreeList
     WHERE n_of_measures < 2
    UNION
    SELECT a.p AS p,
           0
      FROM (
               (
                   SELECT DISTINCT [LEFT] AS p
                     FROM Interdistances
                    WHERE [LEFT] IS NOT NULL
                   UNION
                   SELECT DISTINCT [RIGHT] AS p
                     FROM Interdistances
                    WHERE [RIGHT] IS NOT NULL
               )
               a
               LEFT JOIN
               TreeList b ON a.p = b.p
           )
     WHERE b.p IS NULL;-- View: MissingCoo

CREATE VIEW MissingCoo AS
    SELECT a.*
      FROM TreeList a
           LEFT JOIN
           TreeCoo b USING (
               p
           )
     WHERE b.p IS NULL;-- View: MissingTopology

CREATE VIEW MissingTopology AS
    SELECT a.p,
           b.p
      FROM TreeList a
           LEFT JOIN
           Topology b USING (
               p
           )
     WHERE b.p IS NULL;-- View: Q_Interdistances

CREATE VIEW Q_Interdistances AS
    SELECT m_id,
           [from],
           [to],
           dist
      FROM Interdistances
    UNION
    SELECT m_id,
           [to],
           [from],
           dist
      FROM Interdistances;-- View: SolvableTris

CREATE VIEW SolvableTris AS
    SELECT DISTINCT a0.p AS p3,
                    'l' AS pos,
                    b1.[to] AS p1,
                    c1.[to] AS p2,
                    b1.dist AS p1_p3,
                    b1.m_id,
                    b2.local_x AS P1_x,
                    b2.local_y AS P1_y,
                    c1.dist AS p2_p3,
                    c1.m_id,
                    c2.local_x AS P2_x,
                    c2.local_y AS P2_y
      FROM MissingCoo a0,
           AllTris a1,
           Q_Interdistances b1,
           Q_Interdistances c1,
           TreeCoo b2,
           TreeCoo c2
     WHERE a0.p = a1.pl AND 
           a1.pl = b1.[from] AND 
           a1.p1 = b2.p AND 
           a1.pl = c1.[from] AND 
           a1.p2 = c2.p AND 
           a1.p1 = b1.[to] AND 
           a1.p2 = c1.[to];-- View: Topology

CREATE VIEW Topology AS
    SELECT *
      FROM (
               SELECT m_id,
                      [from],
                      [to],
                      [left] AS p,
                      'l' AS pos
                 FROM Q_Interdistances
                WHERE [left] IS NOT NULL
               UNION
               SELECT m_id,
                      [from],
                      [to],
                      [right] AS p,
                      'r' AS pos
                 FROM Q_Interdistances
                WHERE [right] IS NOT NULL
           )
     WHERE [from] > [to];-- View: TreeList

CREATE VIEW TreeList AS
    SELECT p,
           count( * ) AS n_of_measures
      FROM (
               SELECT [from] AS p,
                      m_id
                 FROM Interdistances
               UNION
               SELECT [to] AS p,
                      m_id
                 FROM Interdistances
           )
     GROUP BY p;

COMMIT TRANSACTION ; 

PRAGMA foreign_keys = on;
