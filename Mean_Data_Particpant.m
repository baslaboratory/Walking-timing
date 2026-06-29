filename = 'participants_randomize_trials.xlsx';
[~, sheets] = xlsfinfo(filename);

distances = [5, 10, 15];
conditions = {'Réel', 'Imaginé'};

for s = 1:length(sheets)
    sheet = sheets{s};

    if strcmp(sheet, 'Feuil1')
        continue;
    end

    T = readtable(filename, 'Sheet', sheet);

    % Vérifie qu'on a au moins 4 colonnes
    if width(T) < 4
        fprintf("⚠️ Feuille '%s' ignorée (moins de 4 colonnes)\n", sheet);
        continue;
    end

    % Extraire colonnes
    conditionCol = T{:,2};
    distanceCol  = T{:,3};
    rawTimeCol   = T{:,4};

    % Convertir temps en numérique
    timeCol = NaN(size(rawTimeCol));
    for i = 1:length(rawTimeCol)
        if iscell(rawTimeCol)
            str = rawTimeCol{i};
            if ischar(str) || isstring(str)
                timeCol(i) = str2double(str);
            elseif isnumeric(str)
                timeCol(i) = str;
            end
        end
    end

    summary = NaN(length(conditions), length(distances));

    for i = 1:length(conditions)
        for j = 1:length(distances)
            mask = strcmp(conditionCol, conditions{i}) & distanceCol == distances(j);
            summary(i,j) = mean(timeCol(mask), 'omitnan');
        end
    end

    % Résumé
    summaryCell = cell(length(conditions)+1, length(distances)+1);
    summaryCell(1,1) = {'Condition'};
    for j = 1:length(distances)
        summaryCell(1,j+1) = {sprintf('%dm', distances(j))};
    end
    for i = 1:length(conditions)
        summaryCell(i+1,1) = {conditions{i}};
        for j = 1:length(distances)
            summaryCell(i+1,j+1) = {summary(i,j)};
        end
    end

    % Écrire dans la feuille
    writecell(summaryCell, filename, 'Sheet', sheet, 'Range', 'F1');
end

disp("✅ Moyennes calculées et insérées avec succès !");